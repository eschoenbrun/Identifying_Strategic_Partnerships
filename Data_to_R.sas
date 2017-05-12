/*Dataset to be loaded into R*/

data review1(keep=user_id business_id);
set review_nc;
run;

data business1(keep=business_id);
set business_nc;
run;

proc sql;
create table business_business as
select distinct business_id,
user_id
from review1;
quit;

proc sql;
create table business_business1 as
select a.*,b.business_id as business_id1
from business_business a left join business_business b
on a.business_id=b.business_id;
quit;

proc sql;
create table business_business2 as
select distinct business_id,
business_id1,
count(distinct user_id) as users
from business_business1
group by 1;
quit;

/*******************/
proc sql;
create table data1 as
select a.*,b.business_id as business_id1
from review1 a left join review1 b
on a.user_id=b.user_id;
quit;

data data2;
set data1;
if business_id=business_id1 then delete;
run;

proc sql;
create table data3 as
select distinct business_id,business_id1,
user_id
from data2;
quit;
