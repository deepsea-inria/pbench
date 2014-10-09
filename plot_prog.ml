   let options_env = Env.from_assoc [
      "pbench_silent", Syntax.Vbool (XCmd.mem_flag "silent");
      "pbench_verbose", Syntax.Vbool (XCmd.mem_flag "verbose");
      "pbench_virtual", Syntax.Vbool (XCmd.mem_flag "virtual");
      ] in
