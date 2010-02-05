%% @author Caleb Tennis <caleb.tennis@gmail.com>
%% @copyright 2010 Data Cave, Inc.  www.thedatacave.com
%% @version 0.9
%% @doc A way to interact with modbus devices on an ethernet network

-module(modbus_device).
-author('Caleb Tennis <caleb.tennis@gmail.com>').

-export([handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).
-export([connect/5,disconnect/1,init/1]).
-export([read_hreg/2,read_hreg/3,read_ireg/2,read_ireg/3,write_hreg/3]).

-include("modbus.hrl").
-behavior(gen_server).

%% @spec connect(Name :: atom(), Type :: atom(), Host:: string(), Port :: integer (), DeviceAddress :: integer () ) -> ok
%% @doc Connect to the remote TCP device
connect(Name, Type, Host, Port, DeviceAddr) ->
	gen_server:start_link({local, Name}, modbus_device, [Type, Host, Port, DeviceAddr],[]).

disconnect(Name) -> gen_server:call(Name, stop).

%% @doc Read from a holding register.
read_hreg(Device, Offset) ->
	gen_server:call(Device, {read_hreg_16, Offset}).
%% @doc Read from a holding register, with optional conversion function of data when finished.
read_hreg(Device,Offset,Fun) ->
	Value = read_hreg(Device,Offset),
	Fun(Value).

read_ireg(Device, Offset) ->
	gen_server:call(Device, {read_ireg_16, Offset}).
read_ireg(Device,Offset,Fun) ->
	Value = read_ireg(Device,Offset),
	Fun(Value).

write_hreg(Device, Offset, Value) ->
	gen_server:call(Device, {write_hreg_16, Offset, Value }).


init([Type, Host, Port, DeviceAddr]) ->
	Retval = gen_tcp:connect(Host, Port, [{active,false}, {packet, 0}]),

	case Retval of
		{ok, Sock} ->
			State = #modbus_state{type=Type,sock=Sock,device_address=DeviceAddr,tid=1},
			{ok, State, 5000};
		{error,ErrorType} ->
			{stop,{error,ErrorType}}
	end.
	
handle_call({read_hreg_16, Offset}, _From, State) ->
	Request = #rtu_request{address=State#modbus_state.device_address,function_code=?FC_READ_HREGS,start=Offset,data=1},
	NewState = State#modbus_state{tid=State#modbus_state.tid + 1},
	{ok, Data} = send_and_receive(NewState,Request),

	[FinalData] = bytes_to_words(Data),

	{reply, FinalData, NewState, 5000};

handle_call({read_ireg_16,Offset}, _From, State) ->
	Request = #rtu_request{address=State#modbus_state.device_address,function_code=?FC_READ_IREGS,start=Offset,data=1},
	NewState = State#modbus_state{tid=State#modbus_state.tid + 1},
	{ok, Data} = send_and_receive(NewState,Request),

	[FinalData] = bytes_to_words(Data),

	{reply, FinalData, NewState, 5000};

handle_call({write_hreg_16,Offset,OrigData}, _From, State) ->
        Request = #rtu_request{address=State#modbus_state.device_address,function_code=?FC_WRITE_HREGS,start=Offset,data=OrigData},
        NewState = State#modbus_state{tid=State#modbus_state.tid + 1},

	{ok, [_Address,_FunctionCode|Data]} = send_and_receive(NewState, Request),

        [FinalData] = bytes_to_words(Data),
            
        {reply, FinalData, NewState, 5000};

handle_call(stop,_From,State) ->
	gen_tcp:close(State#modbus_state.sock),
	{stop, normal, stopped, State}.

handle_cast(_From,State) -> {noreply, State}.

% If we timeout, do a stop
handle_info(timeout,State) ->
	handle_call(stop,whocares,State),
	{stop, normal, State}.

terminate(_Reason,State) -> 
	handle_call(stop,whocares,State).

code_change(_OldVsn, State, _Extra) -> { ok, State }.

send_and_receive(State,Request) ->

	case State#modbus_state.type of
		rtu ->
			TheRequest = Request;
		tcp -> 
			TheRequest = #tcp_request{tid=State#modbus_state.tid,rtu_request=Request}
	end,

	ok = modbus:send_request_message(State,TheRequest),
	ok = modbus:get_response_header(State,TheRequest),
	{ok, _Data} = modbus:get_response_data(State,TheRequest).

% Take a list of modbus bytes, and convert it to a list of words.
bytes_to_words([],Acc)->
  Acc;  
bytes_to_words(Bytes,Acc) ->
  Tail = lists:nthtail(2,Bytes),
  Value = lists:nth(1, Bytes) * 256 + lists:nth(2,Bytes),
  bytes_to_words(Tail,Acc ++ [Value]).
bytes_to_words(Bytes) ->
  bytes_to_words(Bytes,[]).

