-module(cat_test).

-include("erlcat.hrl").
-export([event/0,trans/0,heart/0]).


event()->

	ok.

trans()->
	erlcat:cat_client_init("testapp",#cat_config{}),
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


heart()->
	erlcat:init_cat("testapp",#cat_config{}),
    Data = #{
        "userinfo" => integer_to_list(rand:uniform(1000)),
        "test22" => integer_to_list(rand:uniform(1000)),
        "test333" => integer_to_list(rand:uniform(1000))
    },
    erlcat:log_heartbeat("titleh1",Data),
    ok.
