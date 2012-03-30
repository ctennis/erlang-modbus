%% @author Caleb Tennis <caleb.tennis@gmail.com>
%% @copyright 2010 Data Cave, Inc.  www.thedatacave.com
%% @version 0.9
%% @doc A way to interact with modbus devices on an ethernet network

-module(modbus).
-import(crc16).

-include("modbus.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([send_request_message/2,checksum/1,check_checksum/1,generate_request_message/1,get_response_header/2,get_response_data/2]).

send_request_message(State,Request) ->
	Message =  generate_request_message(Request),
	%io:format("~w~n",[Message]),
	gen_tcp:send(State#modbus_state.sock,Message).

generate_request_message(Request) when is_record(Request, tcp_request) ->
	RtuRequest = Request#tcp_request.rtu_request,
	TID = Request#tcp_request.tid,

	RtuRequestMessage = generate_request_message(RtuRequest),

	case RtuRequestMessage of
		function_not_implemented -> erlang:error(function_not_implemented);
		_ ->

			RtuRequestMessageAsList = binary_to_list(RtuRequestMessage),
			{RtuRequestMessageNoChecksum,_Checksum} = lists:split(length(RtuRequestMessageAsList)-2,RtuRequestMessageAsList),

			RtuReq = list_to_binary(RtuRequestMessageNoChecksum),

			RtuRequestSize = size(RtuReq),

			<<TID:16, 0,0, RtuRequestSize:16, RtuReq/binary>>
	end;

generate_request_message(Request) when is_record(Request, rtu_request) ->
	Message = case Request#rtu_request.function_code of 
		?FC_READ_HREGS ->
			<<(Request#rtu_request.address):8, (Request#rtu_request.function_code):8, (Request#rtu_request.start):16,(Request#rtu_request.data):16>>; 
		?FC_READ_IREGS ->
			<<(Request#rtu_request.address):8, (Request#rtu_request.function_code):8, (Request#rtu_request.start):16,(Request#rtu_request.data):16>>;
		?FC_WRITE_HREGS ->
			Quantity = length(Request#rtu_request.data),
			ValuesBin = list_word16_to_binary(Request#rtu_request.data),
			ByteCount = length(binary_to_list(ValuesBin)),
			<<(Request#rtu_request.address):8, (Request#rtu_request.function_code):8, (Request#rtu_request.start):16,Quantity:16,ByteCount:8,ValuesBin/binary>>;
		_ ->
			erlang:error(function_not_implemented)
	end,

	Checksum = checksum(Message),
	<<Message/binary, Checksum/binary>>.

get_response_header(State,OriginalRequest) when is_record(OriginalRequest, tcp_request) ->
	TID = State#modbus_state.tid,
	_DeviceAddress = State#modbus_state.device_address,
	{ok, [0, TID, 0, 0, 0, _TcpSize]} = gen_tcp:recv(State#modbus_state.sock,6,1000),

	get_response_header(State,OriginalRequest#tcp_request.rtu_request);

get_response_header(State, OriginalRequest) when is_record(OriginalRequest, rtu_request) ->

	% Get RTU header (also encapsulated within TCP)
	{ok, [Address, Code]} = gen_tcp:recv(State#modbus_state.sock,2,1000),

	% io:format("RH A: ~w~n",[Address]),
	% io:format("RH C: ~w~n",[Code]),

	% validate the RTU header->
	OrigAddress = OriginalRequest#rtu_request.address,	
	OrigCode = OriginalRequest#rtu_request.function_code,
	BadCode = OrigCode + 128,

 	case {Address,Code} of
		{OrigAddress,OrigCode} -> ok;
		{OrigAddress,BadCode} -> 
			{ok, [ErrorCode]} = gen_tcp:recv(State#modbus_state.sock,1,1000),

			case ErrorCode of
				1 -> {error, illegal_function};
				2 -> {error, illegal_data_address};
				3 -> {error, illegal_data_value};
				4 -> {error, slave_device_failure};
				5 -> {error, acknowledge};
				6 -> {error, slave_device_busy};
				_ -> {error, unknown_response_code}
			end;
			
		{_,_} -> {error,junkResponse}
  	end.

get_response_data(State,OriginalRequest) when is_record(OriginalRequest, tcp_request) ->
	get_response_data(State,OriginalRequest#tcp_request.rtu_request);

get_response_data(State, OriginalRequest) when is_record(OriginalRequest, rtu_request) ->

	case OriginalRequest#rtu_request.function_code of
		?FC_WRITE_HREGS -> 
			Size = 4,
			ResponseChecksum = [OriginalRequest#rtu_request.address, OriginalRequest#rtu_request.function_code];
		_ ->
			{ok, [Size]} = gen_tcp:recv(State#modbus_state.sock, 1, 1000),
			ResponseChecksum = [OriginalRequest#rtu_request.address, OriginalRequest#rtu_request.function_code, Size]
	end,

	%io:format("Size: ~w~n",[Size]),
	{ok, Data} = gen_tcp:recv(State#modbus_state.sock, Size, 1000),
	%io:format("Data: ~w~n",[Data]),

	% Get and validate the rtu checksum
	case State#modbus_state.type of
		rtu ->
			{ok, Checksum} = gen_tcp:recv(State#modbus_state.sock, 2, 1000),
			% io:format("~w~n",[Checksum]),

			{ok,_} = check_checksum(ResponseChecksum ++ Data ++ Checksum);
		
		_ -> true
	end,

	% Ok, return the data
	{ ok, Data }.


list_word16_to_binary(Values) when is_list(Values) ->
	list_to_binary([<<X:16>> || X <- Values]).

checksum(Data) when is_binary(Data)->
	list_to_binary(checksum(binary_to_list(Data)));
checksum(Data) when is_list(Data) ->
	Value = crc16:calc(Data),
	[Value rem 256, Value div 256].

% Returns true if the last two elements of Data are the checksum
check_checksum(Data) when is_binary(Data) ->
	check_checksum( binary_to_list(Data) );
check_checksum(Data) when is_list(Data) ->
	{Head,Checksum} = lists:split(length(Data)-2,Data),
	ChecksumOfHead = checksum(Head),

	case Checksum of
		ChecksumOfHead -> {ok, Head};
		_ -> {error, checksum }
	end.

request_message_test() ->
	% Modbus RTU tests
	?assertEqual(<<1, 3, 0, 5, 0, 1, 148, 11>>, 
		generate_request_message(#rtu_request{address=1,function_code=?FC_READ_HREGS,start=5,data=1})),

	?assertEqual(<<1, 4, 0, 10, 0, 2, 81, 201>>,
		generate_request_message(#rtu_request{address=1,function_code=?FC_READ_IREGS,start=10,data=2})),

	?assertEqual(<<2, 16, 0, 10, 0, 3, 6, 0, 3, 0, 4, 0, 5, 6, 161>>,
		generate_request_message(#rtu_request{address=2,function_code=?FC_WRITE_HREGS,start=10,data=[3,4,5]})),

	?assertError(function_not_implemented, generate_request_message(#rtu_request{address=1,function_code=0})),


	% Modbus TCP tests
	?assertEqual(<<0, 1, 0, 0, 0, 6, 1, 3, 0, 5, 0, 1>>, 
		generate_request_message(#tcp_request{tid=1, rtu_request=#rtu_request{address=1,function_code=?FC_READ_HREGS,start=5,data=1}})),

	?assertEqual(<<0, 22, 0, 0, 0, 6, 100, 4, 0, 10, 0, 2>>, 
		generate_request_message(#tcp_request{tid=22, rtu_request=#rtu_request{address=100,function_code=?FC_READ_IREGS,start=10,data=2}})),

	?assertError(function_not_implemented, generate_request_message(#tcp_request{tid=22, rtu_request=#rtu_request{address=11,function_code=0}})). 

