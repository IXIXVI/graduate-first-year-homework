math.randomseed(tostring(os.time()):reverse():sub(1, 7))

fp=assert(io.open("bnet_samples.txt","w"))

function I(c)
	n=math.random()
	if (c<=n) then
		return 0;
	else
		return 1;
	end
end

num=1000
aa={}
d1={{0.2,0.8},{0.7,0.9}}
d2={0.1,0.7}
d3=0
d4=0

for i=1,num do
	aa[i]={}
	for j=1,4 do
		aa[i][j]=0
	end
end

for i=1,num do
	aa[i][1]=I(0.7)
	aa[i][2]=I(0.9)
	aa[i][3]=I(d1[aa[i][1]+1][aa[i][2]+1])
	aa[i][4]=I(d2[aa[i][3]+1])
	d3=d3+aa[i][3]
	d4=d4+aa[i][4]
	fp:write(table.concat(aa[i],"\t"),"\n")
end

print(d3/num,d4/num)
