{...}: {
  programs.emacs.init.usePackage = {
    mml-secure = {
      enable = true;
      custom = {
        mml-secure-openpgp-sign-with-sender = true;
        mml-secure-openpgp-signers = "'(\"76A0BD3882DD3E40E9517D59629FC7317EFB4935\")";
      };
    };
    smtpmail = {
      custom = {
        smtpmail-debug-info = true;
      };
      config = ''
        (setq smtpmail-auth-supported '(cram-md5 plain login xoauth2))
      '';
    };
    message = {
      enable = true;
      custom = {
        message-send-mail-function = "'smtpmail-send-it";
      };
    };
    mu4e = {
      enable = true;
      custom = {
        mu4e-get-mail-command = "mbsync";
        mu4e-update-interval = "(* 10 60)"; # 10 min
        mu4e-change-filenames-when-moving = true; # required with mbsync
        mu4e-maildir = "(expand-file-name \"~/Mail\")";
        mu4e-attachment-dir = "(expand-file-name \"~/Downloads\")";
        mu4e-context-policy = "'pick-first";
        mu4e-compose-context-policy = "'ask-if-none";
        mu4e-compose-format-flowed = true;
      };
      config = let
        u = "phdenzel";
        uz = "denp";
        un = "Philipp Denzel";
        gm = "Gmail";
        gmUri = "gmail.com";
        hs = "Hispeed";
        hsUri = "hispeed.ch";
        am = "iCloud";
        amUri = "icloud.com";
        amSri = "mail.me.com";
        ms = "Outlook";
        msUri = "hotmail.com";
        msSri = "outlook.com";
        zh = "ZHAW";
        zhUri = "zhaw.ch";
        zhSri = "office365.com";
        allIBs = "maildir:/Gmail/Inbox OR maildir:/Hispeed/INBOX OR maildir:/iCloud/INBOX OR maildir:/Outlook/Inbox OR maildir:/ZHAW/Inbox";
      in ''
        (mu4e-bookmark-define "${allIBs}" "All Inboxes" ?i)
        (setq mu4e-contexts
          (list
            (make-mu4e-context
              :name "${gm}"
              :match-func
              (lambda (msg) (when msg (string-prefix-p "/${gm}" (mu4e-message-field msg :maildir))))
              :vars '(
                (user-mail-address . "${u}@${gmUri}") (user-full-name . "${un}")
                (message-signature . "PhD") (smtpmail-default-smtp-server . "smtp.${gmUri}")
                (smtpmail-smtp-user . "${u}@${gmUri}") (smtpmail-smtp-server . "smtp.${gmUri}")
                (smtpmail-smtp-service . 587) (smtpmail-stream-type . starttls)
                (mu4e-trash-folder . "/${gm}/[${gm}]/Trash")
                (mu4e-refile-folder . "/${gm}/[${gm}]/All Mail")
                (mu4e-sent-folder . "/${gm}/[${gm}]/Sent Mail")
                (mu4e-drafts-folder . "/${gm}/[${gm}]/Drafts")
                (mu4e-maildir-shortcuts . (("/${gm}/Inbox"             . ?i)
                                           ("/${gm}/[${gm}]/Trash"     . ?t)
                                           ("/${gm}/[${gm}]/All Mail"  . ?a)
                                           ("/${gm}/[${gm}]/Sent Mail" . ?s)
                                           ("/${gm}/[${gm}]/Drafts"    . ?d)
                                           ("/${gm}/[${gm}]/Starred"   . ?f)))
              )
            )

            (make-mu4e-context
              :name "Hispeed"
              :match-func
              (lambda (msg) (when msg (string-prefix-p "/${hs}" (mu4e-message-field msg :maildir))))
              :vars '(
                (user-mail-address . "${u}@${hsUri}") (user-full-name . "${un}")
                (message-signature . "PhD") (smtpmail-default-smtp-server . "smtp.${hsUri}")
                (smtpmail-smtp-user . "${u}@${hsUri}")(smtpmail-smtp-server  . "smtp.${hsUri}")
                (smtpmail-smtp-service . 587)
                (mu4e-trash-folder . "/${hs}/Trash") (mu4e-refile-folder . "/${hs}/Archive")
                (mu4e-sent-folder . "/${hs}/Sent") (mu4e-drafts-folder . "/${hs}/Drafts")
                (mu4e-maildir-shortcuts . (("/${hs}/INBOX"   . ?i)
                                           ("/${hs}/Trash"   . ?t)
                                           ("/${hs}/Archive" . ?a)
                                           ("/${hs}/Sent"    . ?s)
                                           ("/${hs}/Drafts"  . ?d)))
              )
            )

            (make-mu4e-context
              :name "iCloud"
              :match-func
              (lambda (msg) (when msg (string-prefix-p "/${am}" (mu4e-message-field msg :maildir))))
              :vars '(
                (user-mail-address . "${u}@${amUri}") (user-full-name . "${un}")
                (message-signature . "PhD") (smtpmail-default-smtp-server . "smtp.${amSri}")
                (smtpmail-smtp-user . "${u}@${amUri}") (smtpmail-smtp-server . "smtp.${amSri}")
                (smtpmail-smtp-service . 587)
                (mu4e-trash-folder . "/${am}/Deleted Messages")
                (mu4e-refile-folder . "/${am}/Archive")
                (mu4e-sent-folder . "/${am}/Sent Messages") (mu4e-drafts-folder . "/${am}/Drafts")
                (mu4e-maildir-shortcuts . (("/${am}/INBOX"            . ?i)
                                           ("/${am}/Deleted Messages" . ?t)
                                           ("/${am}/Archive"          . ?a)
                                           ("/${am}/Sent Messages"    . ?s)
                                           ("/${am}/Drafts"           . ?d)))
              )
            )

            (make-mu4e-context
              :name "${ms}"
              :match-func
              (lambda (msg) (when msg (string-prefix-p "/Outlook" (mu4e-message-field msg :maildir))))
              :vars '((user-mail-address . "${u}@${msUri}") (user-full-name . "${un}")
                (message-signature . "PhD") (smtpmail-default-smtp-server . "smtp-mail.${msSri}")
                (smtpmail-smtp-user . "${u}@${msUri}") (smtpmail-smtp-server . "smtp-mail.${msSri}")
                (smtpmail-smtp-service . 587) (smtpmail-stream-type  . starttls)
                (mu4e-trash-folder . "/${ms}/Deleted") (mu4e-refile-folder . "/${ms}/Archive")
                (mu4e-sent-folder . "/${ms}/Sent") (mu4e-drafts-folder . "/${ms}/Drafts")
                (mu4e-maildir-shortcuts . (("/${ms}/Inbox"   . ?i)
                                           ("/${ms}/Deleted" . ?t)
                                           ("/${ms}/Archive" . ?a)
                                           ("/${ms}/Sent"    . ?s)
                                           ("/${ms}/Drafts"  . ?d)))
              )
            )

            (make-mu4e-context
              :name "${zh}"
              :match-func
              (lambda (msg) (when msg (string-prefix-p "/${zh}" (mu4e-message-field msg :maildir))))
              :vars '(
                (user-mail-address . "${uz}@${zhUri}") (user-full-name . "${un}")
                (message-signature . "PhD") (smtpmail-default-smtp-server . "smtp.${zhSri}")
                (smtpmail-smtp-user . "${uz}@${zhUri}") (smtpmail-smtp-server . "smtp.${zhSri}")
                (smtpmail-smtp-service . 587) (smtpmail-stream-type . starttls)
                (mu4e-trash-folder . "/${zh}/Deleted Items") (mu4e-drafts-folder . "/${zh}/Drafts")
                (mu4e-sent-folder . "/${zh}/Sent Items") (mu4e-refile-folder . "/${zh}/Archive")
                (mu4e-maildir-shortcuts . (("/${zh}/Inbox"         . ?i)
                                           ("/${zh}/Deleted Items" . ?t)
                                           ("/${zh}/Archive"       . ?a)
                                           ("/${zh}/Sent Items"    . ?s)
                                           ("/${zh}/Drafts"        . ?d)))
              )
            )
          )
        )
      '';
    };
  };
}
