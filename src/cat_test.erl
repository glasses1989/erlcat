-module(cat_test).


-export([event/0,trans/0]).


event()->

	ok.

trans()->
	erlcat:cat_client_init("mpush"),
	send_trans(20),
	ok.

send_trans(0)->
	ok;
send_trans(Index)->
	T1 = erlcat:new_transaction("MSG.send", "send"),
	sleep1(),
	T2 = erlcat:new_transaction("MSG.send", "check"),
	sleep1(),
	erlcat:complete(T2),
	T3 = erlcat:new_transaction("MSG.send", "del111111"),
	sleep1(),
	erlcat:complete(T3),
	erlcat:complete(T1),
	io:format("send ~p~n",[Index]),
	send_trans(Index-1).

sleep1()->

	timer:sleep(rand:uniform(200)).