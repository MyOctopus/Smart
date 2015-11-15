:- use_module(library(aggregate)).

% Each of the different features is partitioned in classes, this means that we will be able to provide the user with textual 
% information about our decision. We could improve the labels, though. Octopus will tell (in the less anoying way as logging
% into the system, so the user can track its decisions):
% + octopus: it was dark and noisy, I decided to lower the lights, you seem to do it very often, about 80% of such occasions! 
% + happy user: hell yeah!

luxometer([dark:[0,0.3],low:[0.3,0.6],normal:[0.6,0.8],high:[0.8,1]]).
temperature([freezing:[-20,5],cold:[5,15],worm:[15,28],hot:[28,40],hell:[40,1000]]).
noise([silent:[0,1],soft:[2,3],regular:[3,5],noisy:[5,7],loud:[7,1000]]).
frequency([armonious:_, rithmic:_]).
% add more labeled features here.

% state/1 contains a list of the measured variables
state([luxometer:ClassesLux,temperature:ClassesTemp,noise:ClassesNoise,frequency:ClassesFreq]) :- 
    luxometer(ClassesLux),temperature(ClassesTemp),noise(ClassesNoise),frequency(ClassesFreq).

% we are controlling lights, so the ranges for the setpoint are those of the luxometer. 
sp(Ranges) :- luxometer(Ranges).

% state/4 will be a foreign call (initially to python, later C) that pulls data from redis. States are pushed discretized. Its last argument infimum(count) is the
% relative count (ranging from -infinite to infinite) of the infimum of the lighling class e.g. infimum(low) = 0.3. It quantifies how
% long has the user set the setpoint closer to the infimum rather than the supremum (e.g. supremum(low)=0.6), this is to balance the
% user preference in the output range, this is to say, the user preferes low lighting under certain conditions (e.g. dark, cold, regular,
% rithmic), then the question here is: how low does he/she like it?
 
%state = [6.5,32,3,armonious].
state(normal,[normal,hot,regular,armonious],8,infimum_count(10)).
 
%state = [6.2,34,3,rithmic].
state(normal,[normal,hot,regular,rithmic],5,infimum_count(20)).
last([normal,hot,regular,rithmic], normal).

setpoint(Class:ICount, SP) :-
    sp(Ranges), member(Class:[Infimum, Supremum], Ranges),
    U is 1/(1 + exp(-ICount)),
    SP is Infimum * U + Supremum * (1 - U),

% this is called automatically by MyOctopus on a changed of observed state.
evaluate_policy(State, Count, Setpoint) :-
    state(DefState),
    discretized(State, DefState, Discretized),
    aggregate(max(C,SPInfo), counts(SPInfo,Discretized,C), max(Count,SPInfo)),
    Class:ICount = SPInfo,
    setpoint(ICount,SP),
    update(SP,Class,State,ICount).

counts(Class:ICount,State,Count) :-
    state(Class,State,Count,ICount), !.

% initially we have a evergy saving policy for every state. We arrive here if there is no
% history about the current state. First time this event(state) is observed, its count will be 1
% so this policy won't be chosen again.  
counts(normal:2,S,0.5). % :- log("The state ",S," was not observed before, setting energy saving policy").

% when punishing the setpoint is given by the user manually.
punish(SP) :-  
    last(S,Class),
    state(Class,S,Count,Inf_Count),
    sp(Ranges), member(Class:[Infimum, Supremum], Ranges),
    SP >= Infimum, SP < Supremum,
    (
	(Infimum - SP) < (SP - Supremum) -> 
	    ICount is Inf_Count + 1; 
	    ICount is Inf_Count - 1
    ),

    New_Count is Count + 1,
    update(SP,Class,S,New_Count,ICount),!.

punish(SP) :-
    class(SP, Ranges, SymClass), 
    state(SymClass,State,Count,ICount),
    sp(Ranges), member(Class:[Infimum, Supremum], Ranges),
    SP >= Infimum, SP < Supremum,
    (
	(Infimum - SP) < (SP - Supremum) -> 
	    ICountNew is ICount + 1; 
	    ICountNew is ICount - 1
    ),
    New_Count is Count + 1,
    update(SP,SymClass,State,New_Count,ICountNew).


  
% discretized(+ReadState,+StateDef,-SymClassesForReadState), all are of the same dimension
%  + ReadState: Numerical or Symbolic values fetched from the sensors
%  + StateDef: List of predefined state variables, each in the format VarName:Ranges 
%  - SymClassedForReadState: The calculated symbolic classes for each variable 

discretized([],[],[]).

% each symbolic variable (which is passed as such in ReadState) must have a variant of discretized that simply passes its value to symclass
discretized([SymClass|RestState],[frequency:_|RestStateDef],[SymClass|RestSymb]) :- 
    discretized(RestState,RestStateDef,RestSymb).

discretized([V|RestState],[X:K|RestStateDef],[SymClass|RestSymb]) :-
    class(V, K, SymClass), 
    discretized(RestState,RestStateDef,RestSymb).

% What is the class this value belongs to
class(Value, K, SymClass) :- 
    % V must be strictly less than the superior in the class
    aggregate(min(DistanceToSup,Class),(member(Class:[_,Sup_Ki], K),V < Sup_Ki,DistanceToSup is Sup_Ki - V), min(_,SymClass)). 
