# vim: set ft=zsh:
#########################
#  My ZSH Prompt Theme  #
#########################
# 
# Uses the pygmalion.zsh-theme as a template.

if [[ -n "$SSH_CLIENT" ]] || [[ -n "$SSH_TTY" ]]; then
    ssh_prompt="%{$fg[white]%}[ssh] %{$reset_color%}"
fi
prompt_setup_pygmalion(){
  ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[green]%}"
  ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
  ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[yellow]%} ⚡%{$reset_color%}"
  ZSH_THEME_GIT_PROMPT_CLEAN=""

  exit_status='%(?..%{$fg[red]%}[%?] %{$reset_color%})'
  base_prompt='%{$fg[magenta]%}%n%{$reset_color%}%{$fg[cyan]%}@%{$reset_color%}%{$fg[yellow]%}%m%{$reset_color%}%{$fg[red]%}:%{$reset_color%}%{$fg[cyan]%}%0~%{$reset_color%}%{$fg[red]%}|%{$reset_color%}'
  post_prompt='%{$fg[cyan]%}⇒%{$reset_color%}  '

  base_prompt_nocolor=$(echo "$base_prompt" | perl -pe "s/%\{[^}]+\}//g")
  post_prompt_nocolor=$(echo "$post_prompt" | perl -pe "s/%\{[^}]+\}//g")

  precmd_functions+=(prompt_pygmalion_precmd)
}

prompt_pygmalion_precmd(){
  local gitinfo=$(git_prompt_info)
  local gitinfo_nocolor=$(echo "$gitinfo" | perl -pe "s/%\{[^}]+\}//g")
  local exp_nocolor="$(print -P \"$base_prompt_nocolor$gitinfo_nocolor$post_prompt_nocolor\")"
  local prompt_length=${#exp_nocolor}

  local nl=""

  if [[ -n "$VIRTUAL_ENV" ]]; then
      venv_prompt="%{$fg[white]%}($(basename $VIRTUAL_ENV)) %{$reset_color%}"
  else
      venv_prompt=
  fi

  if [[ $prompt_length -gt 40 ]]; then
    nl=$'\n%{\r%}';
    PROMPT="$base_prompt$gitinfo $ssh_prompt$venv_prompt$exit_status$nl$post_prompt"
  else
    PROMPT="$exit_status$venv_prompt$ssh_prompt$base_prompt$gitinfo$nl$post_prompt"
  fi
}

prompt_setup_pygmalion
