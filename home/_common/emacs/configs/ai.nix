{config, ...}: let
  light = config.ollama-models.light;
  # Coding and extraction share one model so only one stays loaded
  codingModel =
    if light
    then "qwen3.5:9b-q8_0"
    else "qwen3.6:35b-a3b-coding";
  extractionModel = codingModel;
  extraProviders =
    if light
    then ''
      ("qwen3.5-coding-light" . (make-llm-ollama
                  :chat-model "qwen3.5:9b-q8_0"
                  :embedding-model "embeddinggemma-2:740m"))
    ''
    else ''
      ("qwen3.6-coding" . (make-llm-ollama
                  :chat-model "qwen3.6:35b-a3b-coding"
                  :embedding-model "embeddinggemma-2:740m"))
      ("qwen3.8" . (make-llm-ollama
                  :chat-model "qwen3.8:27b-q8_0"
                  :embedding-model "embeddinggemma-2:740m"))
    '';
in {
  programs.emacs.init.usePackage = {
    copilot = {
      enable = true;
      defer = true;
      after = ["company"];
      config = ''
        (delq 'company-preview-if-just-one-frontend company-frontends)
      '';
    };
    claude-code-ide = {
      enable = true;
      config = ''
        (claude-code-ide-emacs-tools-setup)
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
                :chat-model "gemma4:12b"
                :embedding-model "embeddinggemma-2:740m"
                :default-chat-non-standard-params '(("num_ctx" . 8192))))
        (setopt ellama-summarization-provider
                (make-llm-ollama
                :chat-model "gemma4:12b"
                :embedding-model "embeddinggemma-2:740m"
                :default-chat-non-standard-params '(("num_ctx" . 32768))))
        (setopt ellama-coding-provider
                (make-llm-ollama
                :chat-model "${codingModel}"
                :embedding-model "embeddinggemma-2:740m"
                :default-chat-non-standard-params '(("num_ctx" . 32768))))
        (setopt ellama-providers
               '(${extraProviders}
                 ("gemma4" . (make-llm-ollama
                            :chat-model "gemma4:12b"
                            :embedding-model "embeddinggemma-2:740m"))))
        (setopt ellama-naming-provider
               (make-llm-ollama
               :chat-model "gemma4:12b"
               :embedding-model "embeddinggemma-2:740m"
               :default-chat-non-standard-params '(("stop" . ("\n")))))
        (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
        ;; Translation llm provider
        (setopt ellama-translation-provider
               (make-llm-ollama
               :chat-model "gemma4:12b"
               :embedding-model "embeddinggemma-2:740m"
               :default-chat-non-standard-params '(("num_ctx" . 32768))))
        (setopt ellama-extraction-provider (make-llm-ollama
            :chat-model "${extractionModel}"
            :embedding-model "embeddinggemma-2:740m"
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
