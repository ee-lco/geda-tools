
all: cond.lex.rb cond.tab.rb

%.lex.rb: %.rex
	rex -o $@ $<

%.tab.rb: %.racc
	racc -o $@ $<

