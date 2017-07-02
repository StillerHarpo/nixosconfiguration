with import <nixpkgs> {};

vim_configurable.customize {
  name = "vim";
  vimrcConfig = {
    # custom vimrc
    customRC = ''
      syntax on
      set runtimepath+=/usr/share/vim/vimfiles
      nmap <S-Enter> O<Esc>
      nmap <CR> o<Esc>
      
      """""""""""""""""""""""""""""""""""""""""""""""""
      " REQUIRED. This makes vim invoke Latex-Suite when you 
      " open a tex file.
      filetype plugin on
      
      " IMPORTANT: grep will sometimes skip displaying the 
      " file name if you
      " search in a singe file. This will confuse Latex-Suite.
      " Set your grep
      " program to always generate a file-name.
      set grepprg=grep\ -nH\ $*
      "
      " OPTIONAL: This enables automatic indentation as you 
      " type.
      filetype indent on
      "
      " OPTIONAL: Starting with Vim 7, the filetype of empty 
      " .tex files defaults to
      " 'plaintex' instead of 'tex', which results in vim-latex
      " not being loaded.
      " The following changes the default filetype back 
      " to 'tex':
      let g:tex_flavor='latex'
      """""""""""""""""""""""""""""""""""""""""""""""""
      
      " save last position
      if has("autocmd")
        au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
      endif
      
      " vertical line indentation
      let g:indentLine_color_term = 1 
      let g:indentLine_color_gui = '#09AA08'
      let g:indentLine_char = '│'
      
      set relativenumber
      highlight LineNr ctermbg=002b36
      set conceallevel=0 
     
      "use space for tap 
      set expandtab
      " turn of annoying latex-suite stuff
      let g:Imap_FreezeImap=1
      
      "syntactic
      set statusline+=%#warningmsg#
      set statusline+=%{SyntasticStatuslineFlag()}
      set statusline+=%*
      
      let g:syntastic_always_populate_loc_list = 1
      let g:syntastic_auto_loc_list = 1
      let g:syntastic_check_on_open = 1
      let g:syntastic_check_on_wq = 0
      
      " syntax checkers
      let g:syntastic_haskell_checkers = ['hdevtools', 'hlint']
      let g:syntastic_tex_checkers = []
      "let g:syntastic_tex_chktex_args = "-n all"
      
      "make UltiSnips compatible with ycm
      
      let g:UltiSnipsExpandTrigger="<c-k>"
      let g:UltiSnipsJumpForwardTrigger="<c-b>"
      let g:UltiSnipsJumpBackwardTrigger="<c-z>"
      
      "haskell-vim
      let g:haskell_indent_disable = 1
      
      "you complete me
      let g:ycm_filetype_blacklist = {} 
      
      "spell checking
      setlocal spell spelllang=en,de
      set t_Co=256
      hi clear SpellBad
      hi SpellBad cterm=underline ctermfg=196
      hi clear SyntasticError 
      hi SyntasticError term=reverse ctermfg=15 ctermbg=250
      hi clear SyntasticWarning
      hi SyntasticWarning term=reverse ctermfg=15 ctermbg=11  
    '';
    vam = { 
      knownPlugins = pkgs.vimPlugins;
      pluginDictionaries = [
        {names = [
          "tabular"
          "vim-markdown"
          "UltiSnips"
          "vim-snippets"
          "youcompleteme"
          "latex-box"
          "ctrlp"
          "Syntastic"
          "haskell-vim"
      ];}
     ]; 
    };
  };
}
