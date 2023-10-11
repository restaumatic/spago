module Spago.Command.Bundle
  ( run
  , BundleEnv
  , BundleOptions
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Node.Path as Path
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Types as CSTT
import Spago.Cmd as Cmd
import Spago.Config (BundlePlatform(..), BundleType(..), Workspace, WorkspacePackage)
import Spago.Esbuild (Esbuild)

type BundleEnv a =
  { esbuild :: Esbuild
  , logOptions :: LogOptions
  , bundleOptions :: BundleOptions
  , workspace :: Workspace
  , selected :: WorkspacePackage
  | a
  }

type BundleOptions =
  { minify :: Boolean
  , sourceMaps :: Boolean
  , module :: String
  , outfile :: FilePath
  , platform :: BundlePlatform
  , type :: BundleType
  , extraArgs :: Array String
  }

type RawBundleOptions =
  { minify :: Boolean
  , module :: String
  , outfile :: FilePath
  , platform :: String
  , type :: String
  , extraArgs :: Array String
  }

run :: forall a. Spago (BundleEnv a) Unit
run = do
  { esbuild, selected, workspace, bundleOptions: opts } <- ask
  logDebug $ "Bundle options: " <> show opts
  let
    minify = if opts.minify then [ "--minify" ] else []
    sourceMap = if opts.sourceMaps then [ "--sourcemap" ] else []
    outfile = Path.concat [ selected.path, opts.outfile ]
    format = case opts.platform, opts.type of
      BundleBrowser, BundleApp -> "--format=iife"
      _, _ -> "--format=esm"

    -- See https://github.com/evanw/esbuild/issues/1921
    nodePatch = case opts.platform of
      BundleNode -> [ "--banner:js=import __module from \'module\';import __path from \'path\';import __url from \'url\';const require = __module.createRequire(import.meta.url);const __dirname = __path.dirname(__url.fileURLToPath(import.meta.url));const __filename=new URL(import.meta.url).pathname" ]
      _ -> []

    output = case workspace.buildOptions.output of
      Nothing -> "output"
      Just o -> o
    -- TODO: we might need to use `Path.relative selected.path output` instead of just output there
    mainPath = withForwardSlashes $ Path.concat [ output, opts.module, "index.js" ]

    { input, entrypoint } = case opts.type of
      BundleApp -> { entrypoint: [], input: Cmd.StdinWrite ("#!/usr/bin/env node\n\nimport { main } from './" <> mainPath <> "'; main();") }
      BundleModule -> { entrypoint: [ mainPath ], input: Cmd.StdinNewPipe }
    execOptions = Cmd.defaultExecOptions { pipeStdin = input }

    args =
      [ "--bundle"
      , "--outfile=" <> outfile
      , "--platform=" <> show opts.platform
      -- See https://github.com/evanw/esbuild/issues/1051
      , "--loader:.node=file"
      , format
      ] <> opts.extraArgs <> minify <> sourceMap <> entrypoint <> nodePatch

  -- Before we get into the actual bundling, we need to check if the module that is being bundled has a main function
  -- TODO: get the graph, find the path of the module that is being bundled, call hasMain on it

  logInfo "Bundling..."
  logDebug $ "Running esbuild: " <> show args
  Cmd.exec esbuild.cmd args execOptions >>= case _ of
    Right _r -> logSuccess "Bundle succeeded."
    Left err -> do
      logDebug $ show err
      die [ "Failed to bundle." ]

-- Note: this is coming straight from https://github.com/purescm/purescm/blob/9b87bea57bf6a2d0f412807227a3feca6c37b88f/test/Utils.purs#L94
-- | Returns `Right true` if the source code has this type signature
-- | somewhere in it:
-- | ```
-- | main :: Effect Unit
-- | ```
-- |
-- | If `Effect` or `Unit` are qualified by a module alias,
-- | this will not return `true`.
-- | ```
-- | main :: Effect.Effect Prelude.Unit
-- | ```
hasMain :: String -> Either String Boolean
hasMain sourceCode =
  case parseModule sourceCode of
    ParseSucceeded (CSTT.Module { body: CSTT.ModuleBody { decls } }) ->
      pure $ Array.any isMain decls
    ParseSucceededWithErrors _ errs ->
      Left $ Array.intercalate "\n"
        [ "Could not parse file."
        , Array.intercalate "\n" $ map printPositionedError $ NonEmptyArray.toArray errs
        ]
    ParseFailed err ->
      Left $ Array.intercalate "\n"
        [ "Could not parse file."
        , printPositionedError err
        ]
  where
  printPositionedError err = Array.intercalate "\n"
    [ ""
    , "Position: " <> show err.position
    , "Reason: " <> printParseError err.error
    ]
  isMain = case _ of
    CSTT.DeclSignature
      ( CSTT.Labeled
          { label: CSTT.Name { name: CSTT.Ident "main" }
          , value:
              CSTT.TypeApp
                (CSTT.TypeConstructor (CSTT.QualifiedName { name: CSTT.Proper "Effect" }))
                ( NonEmptyArray
                    [ CSTT.TypeConstructor (CSTT.QualifiedName { name: CSTT.Proper "Unit" })
                    ]
                )
          }
      ) -> true
    _ -> false
