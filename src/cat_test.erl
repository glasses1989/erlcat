-module(cat_test).

-include("erlcat.hrl").
-export([event/0,trans/0,heart/0]).
-export([init/0,trans_count/1,heart_count/1]).


init()->
	erlcat:init_cat("testapp",#cat_config{enable_heartbeat=0,enable_debugLog=1,encoder_type=0}),
	ok.

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

heart_count(0)->
	ok;
heart_count(Count)->
	heart(),
	heart_count(Count-1).

heart()->
	% erlcat:init_cat("testapp",#cat_config{enable_heartbeat=1,enable_debugLog=1}),
    Data = #{
        "userinfo" => integer_to_list(rand:uniform(1000)),
        "test22" => integer_to_list(rand:uniform(1000)),
        "test333" => integer_to_list(rand:uniform(1000))
    },
    erlcat:log_heartbeat("titleh1",Data),
    ok.

trans_count(0)->
	ok;
trans_count(Count)->
	trans_with_due(),
	trans_count(Count-1).

trans_with_due()->
	erlcat:log_transaction_with_duration("TEST","testDuration",rand:uniform(200)).