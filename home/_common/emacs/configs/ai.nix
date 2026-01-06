{...}: {
  programs.emacs.init.usePackage = {
    copilot = {
      enable = true;
      defer = true;
      after = ["company"];
      config = ''
        (delq 'company-preview-if-just-one-frontend company-frontends)
      '';
    };
    aidermacs = {
      enable = true;
      config = ''
        (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
        (setq aider-default-chat-mode 'architect
              aidermacs-backend 'vterm)
      '';
    };
    ellama = {
      enable = true;
      init = ''
        (setopt ellama-language "English")
        (require 'llm-ollama)
        (setopt ellama-provider
                (make-llm-ollama
                ;; this model should be pulled to use it
                ;; value should be the same as you print in terminal during pull
                :chat-model "deepseek-r1:14b"
                :embedding-model "nomic-embed-text"
                :default-chat-non-standard-params '(("num_ctx" . 8192))))
        (setopt ellama-summarization-provider
                (make-llm-ollama
                :chat-model "deepseek-r1:14b"
                :embedding-model "nomic-embed-text"
                :default-chat-non-standard-params '(("num_ctx" . 32768))))
        (setopt ellama-coding-provider
                (make-llm-ollama
                :chat-model "devstral-small-2:24b-instruct-2512-q4_K_M"
                :embedding-model "nomic-embed-text"
                :default-chat-non-standard-params '(("num_ctx" . 32768))))
        (setopt ellama-providers
               '(("deepseek" . (make-llm-ollama
                           :chat-model "deepseek-r1:14b"
                           :embedding-model "deepseek-r1:14b"))
                 ("gpt-oss" . (make-llm-ollama
                            :chat-model "gpt-oss:20b"
                            :embedding-model "gpt-oss:20b"))
                 ("gemma3" . (make-llm-ollama
                            :chat-model "gemma3:27b-it-q4_K_M"
                            :embedding-model "gemma3:27b-it-q4_K_M"))))
        (setopt ellama-naming-provider
               (make-llm-ollama
               :chat-model "llama3.2:3b"
               :embedding-model "nomic-embed-text"
               :default-chat-non-standard-params '(("stop" . ("\n")))))
        (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
        ;; Translation llm provider
        (setopt ellama-translation-provider
               (make-llm-ollama
               :chat-model "deepseek-r1:14b"
               :embedding-model "nomic-embed-text"
               :default-chat-non-standard-params '(("num_ctx" . 32768))))
        (setopt ellama-extraction-provider (make-llm-ollama
            :chat-model "qwen2.5-coder:32b-instruct"
            :embedding-model "nomic-embed-text"
            :default-chat-non-standard-params
            '(("num_ctx" . 32768))))
        ;; customize display buffer behaviour
        ;; see ~(info "(elisp) Buffer Display Action Functions")~
        (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
        (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)'';
      config = ''
        (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message)'';
    };
  };
}
