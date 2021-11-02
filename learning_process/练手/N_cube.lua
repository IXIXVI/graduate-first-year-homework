aa={}
for i=0,10 do
	aa[i]={}
	for j=0,10 do
		aa[i][j]={}
		for k=0,10 do
			aa[i][j][k]=0
		end
	end
end


function JS(n,m,k)
	if aa[n][m][k]~=0 then return aa[n][m][k] end
	if n==0 and m==0 then aa[n][m][k]=1; return 1 end
	if n==0 and k==0 then aa[n][m][k]=1; return 1 end
	if m==0 and k==0 then aa[n][m][k]=1; return 1 end
	for i=0,n-1 do
		if JS(i,m,k)==-1 then aa[n][m][k]=1; return 1 end
	end
	for i=0,m-1 do
		if JS(n,i,k)==-1 then aa[n][m][k]=1; return 1 end
	end
	for i=0,k-1 do
		if JS(n,m,i)==-1 then aa[n][m][k]=1; return 1 end
	end
	return -1
end



print(JS(3,4,5))
