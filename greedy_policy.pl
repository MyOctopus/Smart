:- use_module(library(aggregate)).

% Each of the different features is partitioned in classes, this means that we will be able to provide the user with textual 
% information about our decision. We could improve the labels, though. Octopus will tell (in the less anoying way as logging
% into the system, so the user can track its decisions):
% + octopus: it was dark and noisy, I decided to lower the lights, you seem to do it very often, about 80% of such occasions! 
% + happy user: hell yeah!

luxometer([dark:[0,0.3],low:[0.3,0.6],normal:[0.6,0.8],high:[0.8,1]]).
temperature([freezing:[-20,5],cold:[5,15],worm:[15,28],hot:[28,40],hell:[40,1000]]).
noise([silent:[0,1],soft:[2,3],regular:[3,5],noisy:[5,7],loud:[7,1000]]).
frequency([armonious, rithmic]).
% add more labeled features here.

% we are controlling lights, so the ranges for the setpoint are those of the luxometer. 
sp(Ranges) :- luxometer(Ranges).

% state/4 will be a foreign call (initially to python, later C) that pulls data from redis. Its last argument infimum(count) is the
% relative count (ranging from -infinite to infinite) of the infimum of the lighling class e.g. infimum(low) = 0.3. It quantifies how
% long has the user set the setpoint closer to the infimum rather than the supremum (e.g. supremum(low)=0.6), this is to balance the
% user preference in the output range, this is to say, the user preferes low lighting under certain conditions (e.g. dark, cold, regular,
% rithmic), then the question here is: how low does he/she like it? well here is how we calculate this:

% We would like the range between a=inf(low) and b=sup(low), to this end, let setpoint = a*U + b*(1-U), U is a float value ranging from 0 
% to 1, such that if U = 0 then the setpoint is exactly b (sup(low)=0.6) and U=1 sets to a. So what we need to learn is U. we use here
% a sigmoid (http://www.wolframalpha.com/input/?i=plot+sigmoid+function+x+from+-10+to+10). Now infimum_count is the relative count of the
% times the user has set a setpoint closer to the infimum rather than the supremum. So U =  1 / (1 + e^-infimum_count), so each time the
% infimum is prefered we increse the counter, we decrement it otherwise. Notice that this captures the long-term behavior of the 
% preferences, rather than the short-term one, i.e., say the infimum_count = -50, this means that the user preferes high-low then 
% U ~= 0, so we have setpoint = a*u + b(1-U) ~= b, then unless the user will prefer at least 45 times more supreme(low) where 
% infimum_count will be -5 (look at the plot in the link), the lightning will be rather high-low, this is to say,  people don't changes
% their minds so easyily. 
  
state(7,[6.5,32,3,armonious],8,infimum_count(10)). 
state(6,[6.2,34,3,rithmic],5,infimum_count(20)).

setpoint(Inf_Count, Class, SP) :- U is 1/(1 + exp(-Inf_Count)),
                                  sp(Ranges), member(Class:[Infimum, Supremum], Ranges), 
				  SP is Infimum * U + Supremum * (1 - U).

evaluate_policy(State, Count, Setpoint) :- aggregate(max(C,SP), counts(SP,State,C), max(Count,Setpoint)). 

counts(SP,State,Count) :- % here we don't care about the infimum_count 
    state(SP,State,Count,_),!.

% initially we have a evergy saving policy for every state. We arrive here if there is no
% history about the current state. 
counts(6,_,0.5).
    
 
