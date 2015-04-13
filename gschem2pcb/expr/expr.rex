class ExprParser

# The parent class of this class (RaccParser) now has a 'state' property as well
# => access @state directly instead...
rule
            \$\{        { @state = :VAR; [:LVAR, text] }
    :VAR    \}          { @state = nil;  [:RVAR, text] }
    :VAR    [\w\d_-]+   {                [:IDENTIFIER, text] }
            [^$]+       {                [:PASS, text] }
            \$          {                [:PASS, text] }

end
