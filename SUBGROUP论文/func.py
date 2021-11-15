import numpy as np
from sklearn.linear_model import LinearRegression


def sparation(weight):
    M=len(weight)
    weight1=np.zeros(M+1)
    weight1[0]=0
    weight1[M]=1
    for i in range(1,M):
        weight1[i]=np.sum(weight[:i])
    return weight1


def Beta1(X,Y):
    model = LinearRegression() # 构建线性模型
    model.fit(X, Y) # 自变量在前，因变量在后
    beta0 = model.coef_ # 斜率
    intercept0 = model.intercept_
    return beta0,intercept0


def Beta0(X,Y):
    model = LinearRegression() # 构建线性模型
    model.fit(X, Y) # 自变量在前，因变量在后
    beta0 = model.coef_ # 斜率
    return beta0


'''
fit_intercept：默认True，是否计算模型的截距，为False时，则数据中心化处理
normalize：默认False，是否中心化，或者使用sklearn.preprocessing.StandardScaler()
copy_X：默认True，否则X会被改写
n_jobs：默认为1，表示使用CPU的个数。当-1时，代表使用全部CPU

coef_：训练后的输入端模型系数，如果label有两个，即y值有两列。那么是一个2D的array
intercept_：截距
predict(x)：预测数据
score：评估
'''

def normal(x,theta):
    mu=theta[0]
    s=theta[1]#标准差
    result=(1/(np.sqrt(2*np.pi)*s))*np.exp(-1*(x-mu)**2/(2*s**2))
    return result

def mixnormal(x,theta):
    m=theta.shape[1]
    result=0
    for i in range(m):
        result+=theta[0,i]*normal(x,theta[1:,i])
    return result

def Lih(X,theta):
    num=X.shape[0]
    C=theta.shape[1]
    Ga=np.zeros((num,C))
    c=np.zeros(C)

    for j in range(num):#响应度
        for k in range(C):
            c[k]=theta[0,k]*normal(X[j],theta[1:,k])
        Ga[j]=c/np.sum(c)

    return Ga


def ECM(Xs,Y,betaold,thetaold):
    P=Xs.shape[1]
    num=Xs.shape[0]
    data=Y-np.dot(Xs,betaold.T)
    L0=Lih(data,thetaold)
    C=thetaold.shape[1]
    d=np.zeros(C)
    thetanext=np.zeros((3,C))

    ai=np.zeros(num)
    bi=np.zeros(num)
    xnew=np.zeros((num,P))
    for i in range (num):
        for h in range(C):
            bi[i]+=(L0[i,h])/((thetaold[2,h])**2)
            ai[i]+=((L0[i,h])*((Y[i]-thetaold[1,h])))/((thetaold[2,h])**2)
        xnew[i]=Xs[i]*bi[i]
    betanew=np.dot(np.dot(np.linalg.inv(np.dot(xnew.T,Xs)),Xs.T),ai)

    datanew=Y-np.dot(Xs,betanew.T)
    for k in range(C):#更新参数
        d[k]=np.sum(L0[:,k])
        thetanext[0,k]=d[k]/num
        thetanext[1,k]=np.dot(L0[:,k],datanew[:])/d[k]
        thetanext[2,k]=np.sqrt((np.dot(L0[:,k],(datanew[:]-thetanext[1,k])**2))/(d[k]))#标准差

    return betanew,thetanext


def pl0(Xs,Y,beta,theta):
    n=Xs.shape[0]
    y=np.zeros(n)

    for i in range(n):
        a=Y[i]-np.dot(Xs[i],beta)
        y[i]=np.log(mixnormal(a,theta))
    z=np.sum(y)
    return z



def iterECM(Xs,Y,theta0,N=100,stopbyN= True,stopbyL=False,ε=0.0005):
    beta0=Beta0(Xs,Y)
    T0=[beta0,theta0]
    exitflag=False
    M=theta0.shape[1]

    if stopbyN == True:
        for k in range(N):
            a=T0
            T0=ECM(Xs,Y,T0[0],T0[1])

    elif stopbyL==True:
        for s in range(N):
            al0=pl0(Xs,Y,T0[0],T0[0])
            T0=ECM(Xs,Y,T0[0],T0[1])
            al1=pl0(Xs,Y,T0[0],T0[0])
            a=np.abs(al1-al0)
            if a<=ε:
                break

    else:
        for p in range(N):
            if exitflag == True:
                break
            a=T0
            T0=ECM(Xs,Y,T0[0],T0[1])
            for i in range (3):
                if exitflag == True:
                    break
                for j in range(M):
                    if exitflag == True:
                        break
                    b=np.abs(T0[1][i][j]-a[1][i][j])/(np.abs(a[1][i][j])+ε/5) 
                    if b<= ε:
                        exitflag = True

    return T0


def EM(X,theta):
    num=X.shape[0]
    K=theta.shape[1]
    Ga=np.zeros((num,K))
    c=np.zeros(K)
    d=np.zeros(K)
    thetanext=np.zeros((3,K))
    for j in range(num):
        for k in range(K):
            c[k]=theta[0][k]*normal(X[j],theta[1:,k])
        Ga[j]=c/np.sum(c)

    for k in range(K):
        d[k]=np.sum(Ga[:,k])
        thetanext[0,k]=d[k]/num
        thetanext[1,k]=np.dot(Ga[:,k],X[:])/d[k]
        thetanext[2,k]=np.sqrt((np.dot(Ga[:,k],(X[:]-thetanext[1,k])**2))/(d[k]))#标准差

    return thetanext


def iterEM(Xs,theta0,N=100):
    for k in range(N):
        theta0=EM(Xs,theta0)
    return theta0