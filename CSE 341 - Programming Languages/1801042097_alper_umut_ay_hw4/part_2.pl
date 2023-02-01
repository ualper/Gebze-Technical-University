% -------------------------
% UMUT AY ALPER
% 1801042097
% -------------------------


schedule(istanbul,ankara,1).
schedule(istanbul,izmir,2).           
schedule(izmir,antalya,2).
schedule(erzincan,antalya,3).
schedule(van,gaziantep,3).
schedule(istanbul,rize,4).
schedule(ankara,van,4).
schedule(antalya,diyarbakir,4).
schedule(ankara,rize,5).
schedule(canakkale,erzincan,6).
schedule(izmir,ankara,6).
schedule(ankara,diyarbakir,8).

% CONNECTION
connection(X,Y,Cost) :- schedule(X,Y,Cost). 
connection(X,Y1,Cost) :- schedule(X,Y,Cost1), connection(Y,Y1,Cost2), Cost is Cost1+Cost2.