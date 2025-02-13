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
    ellama = {
      enable = true;
      init = ''
        (setopt ellama-language "English")
        (require 'llm-ollama)
        (setopt ellama-provider
                (make-llm-ollama
                ;; this model should be pulled to use it
                ;; value should be the same as you print in terminal during pull
                :chat-model "llama3.1:8b-instruct-q8_0"
                :embedding-model "nomic-embed-text"
                :default-chat-non-standard-params '(("num_ctx" . 8192))))
        (setopt ellama-summarization-provider
                (make-llm-ollama
                :chat-model "llama3.2:3b"
                :embedding-model "nomic-embed-text"
                :default-chat-non-standard-params '(("num_ctx" . 32768))))
        (setopt ellama-coding-provider
                (make-llm-ollama
                :chat-model "qwen2.5-coder:3b"
                :embedding-model "nomic-embed-text"
                :default-chat-non-standard-params '(("num_ctx" . 32768))))
        (setopt ellama-providers
               '(("zephyr" . (make-llm-ollama
                           :chat-model "zephyr:7b-beta-q6_K"
                           :embedding-model "zephyr:7b-beta-q6_K"))
                  ("deepseek" . (make-llm-ollama
                           :chat-model "deepseek-r1:7b"
                           :embedding-model "deepseek-r1:7b"))
                 ("mixtral" . (make-llm-ollama
                            :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M"
                            :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M"))))
        (setopt ellama-naming-provider
               (make-llm-ollama
               :chat-model "llama3.1:8b-instruct-q8_0"
               :embedding-model "nomic-embed-text"
               :default-chat-non-standard-params '(("stop" . ("\n")))))
        (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
        ;; Translation llm provider
        (setopt ellama-translation-provider
               (make-llm-ollama
               :chat-model "qwen2.5:3b"
               :embedding-model "nomic-embed-text"
               :default-chat-non-standard-params '(("num_ctx" . 32768))))
        (setopt ellama-extraction-provider (make-llm-ollama
            :chat-model "qwen2.5-coder:7b-instruct-q8_0"
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
