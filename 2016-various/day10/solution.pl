%
test:-
	parse([
		"value 5 goes to bot 2",
		"bot 2 gives low to bot 1 and high to bot 0",
		"value 3 goes to bot 1",
		"bot 1 gives low to output 1 and high to bot 0",
		"bot 0 gives low to output 2 and high to output 0",
		"value 2 goes to bot 2"
	], (Bots,Sends)),
	apply(Bots, Sends, Compared, Outputs),
	who_compared(Compared, 5, 2, 2),
	multiplied(Outputs, 30).

parse(Lines, Out):- parse(Lines, (bots{},[]), Out).
parse([], Out, Out).
parse([Line|Lines], (BotsIn,SendsIn), Out):-
	split_string(Line, " ", "", Parts),
	(
		parse_bot(Parts,BotsIn,BotsOut),SendsOut=SendsIn;
		parse_send(Parts,SendsIn,SendsOut),BotsOut=BotsIn
	),
	parse(Lines, (BotsOut,SendsOut), Out).

parse_bot(["bot",NumStr,_,_,_,LKindStr,LNumStr,_,_,_,HKindStr,HNumStr], In, Out):-
	number_string(Num, NumStr),
	parse_destination(LKindStr, LNumStr, LDest),
	parse_destination(HKindStr, HNumStr, HDest),
	Out=In.put(Num, bot{lo:LDest, hi:HDest}).

parse_send(["value",ValueStr,_,_,_,NumStr], In, Out):-
	number_string(Num, NumStr),
	number_string(Value, ValueStr),
	Out=[send{value:Value,bot:bot(Num)}|In].

parse_destination("bot", NumStr, D):- number_string(Num, NumStr), D=bot(Num).
parse_destination("output", NumStr, D):- number_string(Num, NumStr), D=output(Num).

apply(Bots, Sends, Compared, Outputs):-
	apply_rec(Bots, Sends, (payloads{},compared{},outputs{}), (Compared, Outputs)).
apply_rec(_, [], (_,Compared,Outputs), (Compared,Outputs)).
apply_rec(Bots, [Send|Sends], In, Out):-
	update(Bots, Send.value, Send.bot, In, Updated),
	apply_rec(Bots, Sends, Updated, Out).

update(Bots, Value, bot(Num), (PIn,CIn,OIn1), (POut,COut,OOut)):-
	(CT=CIn.get(Num),C=[Value|CT];C=[Value]),
	CIn1=CIn.put(Num, C),
	(P=PIn.get(Num) ->
		del_dict(Num, PIn, _, PIn1),
		Bot=Bots.get(Num),
		min_of(Value, P, Lo),
		max_of(Value, P, Hi),
		update(Bots, Lo, Bot.lo, (PIn1,CIn1,OIn1), (PIn2,CIn2,OIn2)),
		update(Bots, Hi, Bot.hi, (PIn2,CIn2,OIn2), (POut,COut,OOut))
		;
		POut=PIn.put(Num, Value),
		COut=CIn1,
		OOut=OIn1
	).

update(_, Value, output(Num), (P,C,OIn), (P,C,OOut)):-
	OOut=OIn.put(Num, Value).

min_of(A,B,O):- A<B, O=A;O=B.
max_of(A,B,O):- A>B, O=A;O=B.

who_compared(C, A, B, Who):-
	C.get(Who)=[A,B];
	C.get(Who)=[B,A].

multiplied(O, P):-
	P is O.get(0) * O.get(1) * O.get(2).

main:-
	open("./input", read, Stream),
	read_lines(Stream, Lines),
	close(Stream),
	parse(Lines, (Bots,Sends)),
	apply(Bots, Sends, Compared, Outputs),
	who_compared(Compared, 61, 17, Who),
	multiplied(Outputs, P),
	format("part1: ~p ~n", [Who]),
	format("part2: ~p ~n", [P]).

read_lines(Stream, []):-
	at_end_of_stream(Stream).
read_lines(Stream, [Line|Lines]):-
	\+ at_end_of_stream(Stream),
	read_line_to_codes(Stream, Codes),
	atom_chars(Line, Codes),
	read_lines(Stream, Lines).
