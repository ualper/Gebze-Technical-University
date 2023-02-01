
% -------------------------
% UMUT AY ALPER
% 1801042097
% -------------------------


% rooms
room_id(room1,1).
room_id(room2,2).

room_capacity(room1,100).
room_capacity(room2,200).

room_equipment(white_board,room1).
room_equipment(projector,room2).

% courses
course_in_student(student1,course1).
course_in_student(student2,course2).
course_class_hour(13,course1,room1).
course_class_hour(14,course1,room1).
course_class_hour(15,course2,room1).  
course_class_hour(15,course2,room1).  % shold be conflict




%instructor
course_instructor(101,instructor1).
preference(projector,instructor1).


% student
student(101,student1).
student(102,student2).


% predicates

free_hour(Room,X) :- (course_class_hour(Hour,_,Room);hours(X) ) , X \= Hour.  


% find conflict in schedule
conflict_in_schedule() :- course_class_hour(A,B,C), course_class_hour(A,Z,C), B \= Z, write('Conflict in schedule').
