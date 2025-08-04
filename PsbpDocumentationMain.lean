import Std.Data.HashMap
import VersoManual
import PsbpDocumentation.PsbpDocumentation

open Verso Doc
open Verso.Genre Manual

open Std (HashMap)

open PsbpDocumentation

def config : Config where
  emitTeX := false
  emitHtmlSingle := true
  emitHtmlMulti := true
  htmlDepth := 2

def main := manualMain (%doc PsbpDocumentation) (config := config)
