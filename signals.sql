with recursive test_ema(code, date, rn, adj_close, ema12, ema26) as (
    with test as (
	select code, date, row_number() over(partition by code order by date) as rn, adj_close from stock)
	select code, date, rn, adj_close, adj_close as ema12, adj_close as ema26 from test where rn = 1
    union all
    select a.code, a.date, a.rn, a.adj_close, a.adj_close*2/13 + b.ema12 * 11/13 as ema12,
    a.adj_close*2/27 + b.ema26 * 25/27 as ema26
    from test a inner join test_ema b on a.rn = b.rn + 1 and a.code = b.code
)
create table signal_test as (select *, ema12-ema26 as macd from test_ema order by code, date);