
all: expr.lex.rb expr.tab.rb

%.lex.rb: %.rex
	rex -o $@ $<

%.tab.rb: %.racc
	racc -o $@ $<

