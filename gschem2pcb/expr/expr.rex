class ExprParser
rule
            \$\{        { state = :VAR; [:LVAR, text] }
    :VAR    \}          { state = nil;  [:RVAR, text] }
    :VAR    [\w\d_-]+   {               [:IDENTIFIER, text] }
            [^$]+       {               [:PASS, text] }
            \$          {               [:PASS, text] }

end
