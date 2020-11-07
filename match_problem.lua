xx={1,0}
function Rand()
    xx[2]=xx[2]*45221+xx[1]*4793
    xx[1]=xx[1]*45221+453806245
    xx[2]=xx[2]+(xx[1]-xx[1]%65536)/65536
	xx[2]=xx[2]%32768;    xx[1]=xx[1]%65536
    return (xx[2]*65536+xx[1])/2147483648
end

ss={}
Di=1000
Sn=10000
for i= 1,Sn do
	ss[i]={}
	for j=1,Di do
	    ss[i][j]=Rand()
    end


end

fp=assert(io.open("distance_samples.txt","w"))

for i=1,Sn-1 do
	for k=i+1,Di do
		m=0
		for j=1,Di do
			m=m+(ss[i][j]-ss[k][j])^2
		end
		n=math.sqrt(m)
		fp:write(n,"\n")
	end
end
