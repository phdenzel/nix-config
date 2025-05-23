{pkgs, ...}: {
  home.file.".config/rofi-pass/config".text = ''
    pw_store_dir="$HOME/.password-store"
    clipboard_backend="wl-clipboard"
    clipboard_timeout=30
    clipboard='both'
    notifications=1
  '';

  home.packages = with pkgs; [
    (writeShellScriptBin "rofi-pass" ''
# rofi-pass
# (c) 2025 Philipp Denzel <phdenzel@gmail.com>
basecommand="$0"

config_dir=''${XDG_CONFIG_HOME:-$HOME/.config}
cache_dir=''${XDG_CACHE_HOME:-$HOME/.cache}
pw_store_dir=''${PASSWORD_STORE_DIR:-$HOME/.password-store}
pw_prompt=''${ROFI_PASS_DISPLAY:-" "}
pw_key=
clipboard_backend=''${ROFI_PASS_CLIPBOARD_BACKEND:-"wl-clipboard"}
clipboard_timeout=45
clipboard='primary'
notifications=0

# safe permissions
umask 077

read -r -d ''' usage <<-EOF
Usage: rofi-pass [-d|--password-store PATH]
                 [-h|--help]

       Rofi alternative to passmenu

       -h, --help              Prints this message.
       -c, --config PATH       Path to the config file.
       -p, --password-store PATH
                               Sets the path where the password-store is installed.
                               [Default: \$PASSWORD_STORE_DIR]
       -n, --notify            Send notifications which password was selected.
       -t, --timeout TIMEOUT   Set the clipboard timeout. [Default: 45]
       -b, --backend BACKEND
                               Set the clipboard backend. [Default: wl-clipboard]
       -g, --get KEY           Choose the key to fetch from the password file, e.g.
                               'username', 'email', 'url', etc. [Default: 'password']
EOF

# set default settings
_rofi () {
    rofi -no-auto-select -i "$@"
}

_clip_in_primary() {
    case "$clipboard_backend" in
        "xclip")
            xclip
            ;;
        "wl"*)
            wl-copy -p
            ;;
        *)
            if command -v wl-copy &> /dev/null; then
               wl-copy -p
            else
               xclip
            fi
            ;;
    esac
}

_clip_in_clipboard() {
    case "$clipboard_backend" in
	      "xclip")
            xclip -selection clipboard
            ;;
	      "wl"*)
            wl-copy
            ;;
	      *)
            if command -v wl-copy &> /dev/null; then
                wl-copy
            else
                xclip -selection clipboard
            fi
		        ;;
    esac
}

_clip_out_primary() {
    case "$clipboard_backend" in
	      "xclip")
            xclip -o
            ;;
	      "wl"*)
            wl-paste -p
            ;;
	      *)
            if command -v wl-copy &> /dev/null; then
                wl-paste -p
            else
                xclip -selection clipboard -o
            fi
		        ;;
    esac
}

_clip_out_clipboard() {
    case "$clipboard_backend" in
	      "xclip")
            xclip -selection clipboard -o
            ;;
	      "wl"*)
            wl-paste
            ;;
	      *)
            if command -v wl-copy &> /dev/null; then
                wl-paste
            else
                xclip -selection clipboard -o
            fi
		        ;;
    esac
}


# utility functions

_get_config_file() {
    configs=("$ROFI_PASS_CONFIG"
             "$config_dir/rofi-pass/"*conf*
             "/etc/rofi-pass.conf")
    for config in "''${configs[@]}"; do
        if [[ -n "$config" && -f "$config" ]]; then
            printf "%s" "$config"
            return
        fi
    done
}

_listgpg () {
	  mapfile -d ''' pw_list < <(find -L . -name '*.gpg' -print0)
	  pw_list=("''${pw_list[@]#./}")
	  printf '%s\n' "''${pw_list[@]}" | sort -n
}

_list_passwords () {
    cd "''${pw_store_dir}" || exit
	  mapfile -t pw_list < <(_listgpg)
	  printf '%s\n' "''${pw_list[@]%.gpg}" | sort -n
}

_clip() {
    case "$clipboard" in
        "primary")
            _clip_in_primary
            ;;
        "clipboard")
            _clip_in_clipboard
            ;;
        "both")
            _clip_in_primary;
            _clip_out_primary | _clip_in_clipboard
            ;;
    esac
}


# main function(s)

select_password () {
    # Use rofi to select the password file
    unset password_file
    # rofi flags
    args=( -dmenu
           #-kb-mode-previous ""
           #-kb-mode-next ""
		       -p "$pw_prompt")
    password_file="$(_list_passwords 2>/dev/null | _rofi "''${args[@]}")"
    printf "%s" "$password_file"
}


parse_password () {
    # Use pass to read and parse the password file
    mapfile -t password_temp < <(PASSWORD_STORE_DIR="''${root}" pass show "$password_file")
    password=''${password_temp[0]}
    metadata=$(printf '%s\n' "''${password_temp[@]:1}")

    # sort into associative array
    declare -A fields
    fields['file']="$password_file"
    if [[ "$password" = "otpauth://"* ]]; then
        fields['otp']="''${password}"
    else
        fields['password']="''${password}"
    fi
    if [[ -n $metadata ]]; then
        while read -r LINE; do
				    unset _id _val
            case "$LINE" in
					      "otpauth://"*)
						        _id="otp"
						        _val="$LINE"
						        ;;
					      *)
						        _id="''${LINE%%: *}"
						        _val="''${LINE#* }"
						        ;;
				    esac
            if [[ -n "$_id" ]]; then
					      fields["''${_id}"]=''${_val}
				    fi
			  done < <(printf '%s\n' "''${metadata}")
    fi

    # fetch the requested field
    if [ -n "''${pw_key}" ] && [ "''${fields[$pw_key]}" ]; then
        password_field="''${fields[$pw_key]}"
    elif ([ "''${pw_key}" = 'otp' ] && [ "''${fields[otp]}" ]) || [ "''${fields[otp]}" ]; then
        password_field="$(pass otp ''${fields['file']})"
    else
        password_field="''${fields['password']}"
    fi

    printf "%s" "$password_field"

    unset password_temp
    unset password
    unset metadata
    unset fields
}


copy_password () {
    # copy password to clipboard
    printf '%s' "$password" | _clip;
    if [[ $notifications -eq 1 ]]; then
        notify-send "rofi-pass" "Copied password ''${password_file}\\nClearing in ''${clipboard_timeout} seconds"
    fi

    # launch delayed clear up
    if [[ $notifications -eq 1 ]]; then
		    (sleep $clipboard_timeout;
         printf '%s' "" | _clip_in_primary;
         printf '%s' "" | _clip_in_clipboard | notify-send "rofi-pass" "Clipboard cleared") &
	  else
		    (sleep $clipboard_timeout;
         printf '%s' "" | _clip_in_primary;
         printf '%s' "" | _clip_in_clipboard) &
	  fi
}


main () {
    # set defaults
    config_file="$(_get_config_file)"
    [[ -n "$config_file" && -f "$config_file" ]] && source "$config_file"
    # parse args
    while [ $# -ge 1 ]; do
        case "$1" in
            -h|--help)
                echo "$usage"
                exit 0
                ;;
            -c|--config)
                shift
                [[ -f "$1" ]] && config_file="$1" && source "$config_file"
                ;;
            -d|--password-store|--password_store)
                shift
                pw_store_dir="$1"
                ;;
            -n|--notify)
                notifications=1
                ;;
            -t|--timeout)
                shift
                clipboard_timeout="$1"
                ;;
            -b|--backend)
                shift
                clipboard_backend="$1"
                ;;
            -g|--get)
                shift
                pw_key="$(echo $1 | tr '[:upper:]' '[:lower:]')"
                ;;
            *)
                # unknown option
                ;;
        esac
        shift # past argument or value
    done

    # select password file
    password_file="$(select_password)"
    if [[ -z "$password_file" ]]; then
        exit
    fi

    # fetch requested field from the password file
    password="$(parse_password)"

    # copy password to clipboard
    copy_password;

    unset password_file
    unset password

}

main "$@"
    '')
  ];
}
