# erlcat
erlang cat client

## Feature
whole cat client api 

## Usage

```erlang
%% setup cat client evn
erlcat:init_cat("testapp",#cat_config{enable_heartbeat=1,enable_debugLog=1,encoder_type=1}),

%% new process context, each process must generator a context. 
ErlCatContext = erlcat:new_context(),

%% log transction
T1 = erlcat:new_transaction(ErlCatContext, "MSG.send", "test"),    
erlcat:complete(ErlCatContext, T1).

```

## API

reference the module `erlcat`
