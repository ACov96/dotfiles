function fish_title
    true
end

set -gx PATH ~/.local/Postman/ ~/.local/go/bin $PATH
set -gx CLASSPATH . /user/local/lib/antler-4.7.1-complete.jar
alias antlr4='java -Xmx500M -cp "/usr/local/lib/antlr-4.7.1-complete.jar:$CLASSPATH" org.antlr.v4.Tool'
alias grun='java -Xmx500M -cp "/usr/local/lib/antlr-4.7.1-complete.jar:$CLASSPATH" org.antlr.v4.gui.TestRig'

eval (python3 -m virtualfish)
