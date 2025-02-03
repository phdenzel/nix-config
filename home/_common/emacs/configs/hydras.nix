{...}: {
  programs.emacs.init.usePackage = {
    hydra = {
      enable = true;
      hydras = {
        "phd/hydra-flycheck" = {
          color = "blue";
          hint = "none";
          description = ''
            ^
            ^Flycheck^          ^Errors^            ^Checker^
            ^────────^──────────^──────^────────────^───────^─────
            _q_ quit            _p_ previous        _?_ describe
            _M_ manual          _n_ next            _d_ disable
            _!_ verify setup    _f_ check           _m_ mode
            ^^                  _l_ list            _s_ select
            _v_ hint            ^^                  ^^
          '';
          binds = {
            "Q" = "nil";
            "q" = "nil";
            "C-g" = "nil";
            "v" = "(phd/hydra-hint-toggle 'phd/hydra-flycheck 'phd/hydra-flycheck/body) :exit nil";
            "p" = "flycheck-previous-error :color pink";
            "n" = "flycheck-next-error :color pink";
            "?" = "flycheck-describe-checker";
            "M" = "flycheck-manual";
            "d" = "flycheck-disable-checker";
            "f" = "flycheck-buffer";
            "l" = "flycheck-list-errors";
            "m" = "flycheck-mode";
            "s" = "flycheck-select-checker";
            "!" = "flycheck-verify-setup";
          };
        };
        "phd/hydra-llm" = {
          color = "blue";
          hint = "none";
          description = ''
            ^
            ^AI LLM Hydra^
            ^──────────────^─^────────────^─^──────────────^─^───
            _a_ Chat        _c_ Code       _t_ Translation
            _m_ TabbyML     _i_ Improve    _s_ Summarize
            _p_ Providers   _x_ Context    _s_ Session
          '';
          binds = {
            "Q" = "nil";
            "q" = "nil";
            "C-g" = "nil";
            "v" = "(phd/hydra-hint-toggle 'phd/hydra-llm 'phd/hydra-llm/body) :exit nil";
            "a" = "phd/hydra-llm-chat/body";
            "c" = "phd/hydra-llm-code/body";
            "t" = "phd/hydra-llm-translation/body";
            "i" = "phd/hydra-llm-improve/body";
            "x" = "phd/hydra-llm-context/body";
            "s" = "phd/hydra-llm-session/body";
            "m" = "phd/hydra-tabby-ml/body";
            "p" = "ellama-provider-select";
          };
        };
        "phd/hydra-llm-chat" = {
          color = "blue";
          hint = "none";
          description = ''
            ^
            ^Ellama Chat^
            ^───────────^^────────────^^────────────^^─────────
            _a_ ask      _i_ chat      _l_ line      _s_ region
          '';
          binds = {
            "Q" = "nil";
            "q" = "nil";
            "C-g" = "nil";
            "v" = "(phd/hydra-hint-toggle 'phd/hydra-llm-chat 'phd/hydra-llm-chat/body) :exit nil";
            "a" = "ellama-ask-about";
            "i" = "ellama-chat";
            "l" = "ellama-ask-line";
            "s" = "ellama-ask-selection";
          };
        };
        "phd/hydra-llm-code" = {
          color = "blue";
          hint = "none";
          description = ''
            ^
            ^Ellama Code   ^
            ^──────────────^^─────────^^──────────^^─────────────^^────────────^^────────^──
            _c_ complete    _a_ add    _e_ edit    _i_ improve    _r_ review    _m_ commit
          '';
          binds = {
            "Q" = "nil";
            "q" = "nil";
            "C-g" = "nil";
            "v" = "(phd/hydra-hint-toggle 'phd/hydra-llm-code 'phd/hydra-llm-code/body) :exit nil";
            "c" = "ellama-code-complete";
            "a" = "ellama-code-add";
            "e" = "ellama-code-edit";
            "i" = "ellama-code-improve";
            "r" = "ellama-code-review";
            "m" = "ellama-generate-commit-message";
          };
        };
        "phd/hydra-llm-translation" = {
          color = "blue";
          hint = "none";
          description = ''
            ^
            ^Ellama Translate^
            ^────────────────^^─────────────^^───────────────^^─────────────^^──────────^─
            _t_ translate     _b_ buffer     _c_ complete     _e_ enable     _d_ disable
          '';
          binds = {
            "Q" = "nil";
            "q" = "nil";
            "C-g" = "nil";
            "v" = "(phd/hydra-hint-toggle 'phd/hydra-llm-translation 'phd/hydra-llm-translation/body) :exit nil";
            "t" = "ellama-translate";
            "b" = "ellama-translate-buffer";
            "c" = "ellama-complete";
            "e" = "ellama-chat-translation-enable";
            "d" = "ellama-chat-translation-disable";
          };
        };
        "phd/hydra-llm-improve" = {
          color = "blue";
          hint = "none";
          description = ''
            ^
            ^Ellama Improve^
            ^─────────────^^─────────────^^──────────────^─
            _w_ wording    _g_ grammar    _c_ conciseness
          '';
          binds = {
            "Q" = "nil";
            "q" = "nil";
            "C-g" = "nil";
            "v" = "(phd/hydra-hint-toggle 'phd/hydra-llm-improve 'phd/hydra-llm-improve/body) :exit nil";
            "w" = "ellama-improve-wording";
            "g" = "ellama-improve-grammar";
            "c" = "ellama-improve-conciseness";
          };
        };
        "phd/hydra-llm-session" = {
          color = "blue";
          hint = "none";
          description = ''
            ^
            ^Ellama Sessions / Summarize^
            ^──────────^^───────────────^^──────────────^^────────────^^─────────^─
            Session:   ^^_l_ load        _r_ rename      _d_ delete    _a_ switch
            Summarize: ^^_s_ summarize   _w_ webpage     _c_ killring ^^
          '';
          binds = {
            "l" = "ellama-load-session";
            "r" = "ellama-session-rename";
            "d" = "ellama-session-remove";
            "a" = "ellama-session-switch";
            "s" = "ellama-summarize";
            "w" = "ellame-summarize-webpage";
            "c" = "ellama-summarize-killring";
          };
        };
        "phd/hydra-llm-context" = {
          color = "blue";
          hint = "none";
          description = ''
            ^
            ^Ellama Context^
            ^────────────^^──────────^^────────────^^───────^─
            _b_ buffer    _f_ file    _s_ region    _i_ info
          '';
          binds = {
            "b" = "ellama-context-add-buffer";
            "f" = "ellama-context-add-file";
            "s" = "ellama-context-add-selection";
            "i" = "ellama-context-add-info-node";
          };
        };
      };
    };
  };
}
