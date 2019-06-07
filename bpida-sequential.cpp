#include <bits/stdc++.h>
//#include <cuda.h>

#define INF 10000
#define maxLimitF 80

using namespace std;

#define IDX(i, j) ((i << 2) + j << 2)
#define SUB(m, t) (((1ll << t+4) - (1ll << t) & m) >> t)

const int MV = 4;
const int SZ = 4;
//const int ThreadsPerBlock = 512;
const int stackSize = (1 << 10);
const int cutoff = (1 << 9) + (1 << 8);

struct Node
{
    uint8_t type;
    Node *p;
};

struct S
{
    uint64_t m;
    uint16_t x, y, g, h;
    Node *n;

    void setH()
    {
        h = 0;
        for(int i=0; i<SZ; i++)
        {
            for(int j=0; j<SZ; j++)
            {
                int t = SUB(m, IDX(i, j));
                if(t != 15)
                    h += abs(i - (t >> 2)) + abs(j - (t & 3));
            }
        }
    }

    void init()
    {
        m = 0;
        for(int i=0; i<SZ; i++)
        {
            for(int j=0; j<SZ; j++)
            {
                uint64_t t;
                cin >> t;
                t--;
                if(t == -1)
                    t = 15;
                m |= (t << ((i << 2) + j << 2));
                if(t == 15)
                {
                    x = i;
                    y = j;
                }
            }
        }
        g = 0;
        n = NULL;
        setH();
    }

    /*__device__*/ bool move(int type)
    {
        if(type == 0)
        {
            if(x == 0)
                return false;
            uint64_t t = SUB(m, IDX(x-1, y));
            h += (1 - (((t >> 2) >= x) << 1));
            m ^= (15ll << ((x << 2) + y << 2));
            m |= (t << ((x << 2) + y << 2));
            x--;
            m |= (15ll << ((x << 2) + y << 2));
        }
        else if(type == 1)
        {
            if(x == 3)
                return false;
            uint64_t t = SUB(m, IDX(x+1, y));
            h += (1 - (((t >> 2) <= x) << 1));
            m ^= (15ll << ((x << 2) + y << 2));
            m |= (t << ((x << 2) + y << 2));
            x++;
            m |= (15ll << ((x << 2) + y << 2));
        }
        else if(type == 2)
        {
            if(y == 0)
                return false;
            uint64_t t = SUB(m, IDX(x, y-1));
            h += (1 - (((t & 3) >= y) << 1));
            m ^= (15ll << ((x << 2) + y << 2));
            m |= (t << ((x << 2) + y << 2));
            y--;
            m |= (15ll << ((x << 2) + y << 2));
        }
        else if(type == 3)
        {
            if(y == 3)
                return false;
            uint64_t t = SUB(m, IDX(x, y+1));
            h += (1 - (((t & 3) <= y) << 1));
            m ^= (15ll << ((x << 2) + y << 2));
            m |= (t << ((x << 2) + y << 2));
            y++;
            m |= (15ll << ((x << 2) + y << 2));
        }
        else
            return false;
        g++;
        Node *nn = (Node *)malloc(sizeof(Node));
        nn->p = n;
        nn->type = type;
        n = nn;
        return true;
    }
};

struct Q
{
    S s;
    int moveType;
};

/*__global__*/ void BPIDA(S *rootSet, int *goalSeq, int *finalG, int *flag)
{
    int idx = 0; //threadIdx.x;
     /*__shared__*/ int limitF;
     /*__shared__*/ int newLimitF;
     /*__shared__*/ int ss;
     /*__shared__*/ Q st[stackSize];
     /*__shared__*/ Q *stPtr;
     /*__shared__*/ S goalNode;
    Q q;
    if(idx == 0)
    {
        ss = 0;
        newLimitF = 0;
        stPtr = st;
    }

    while(*flag == -1)
    {
        //__syncthreads();
        if(ss == 0)
        {
            if(idx == 0)
            {
                limitF = newLimitF;
                newLimitF = INF;
                if(limitF > maxLimitF)
                    *flag = -2;
                else
                {
                    q.s = rootSet[0]; //blockIdx.x];
                    for(int i=0; i<MV; i++)
                    {
                        q.moveType = i;
                        *stPtr++ = q;
                    }
                    ss = MV;
                }
            }
            continue;
        }
        bool f = false;
        if(idx < ss and (ss < cutoff or idx == 0))
        {
            q = *--stPtr;
            f = true;
        }
        //__syncthreads();
        if(f)
        {
            ss--;
            int mt = q.moveType;
            if(q.s.move(mt))
            {
                int f = q.s.h + q.s.g;
                if(f <= limitF)
                {
                    if(q.s.h == 0)
                    {
                        *flag = 0; //blockIdx.x;
                        goalNode = q.s;
                    }
                    else
                    {
                        mt ^= 1;
                        for(int i=0; i<MV; i++)
                        {
                            if(i != mt)
                            {
                                q.moveType = i;
                                *stPtr++ = q;
                                ss++;
                            }
                        }
                    }
                }
                else
                    newLimitF = min(newLimitF, f);
            }
        }
    }

    if(/*blockIdx.x*/ 0 == *flag and idx == 0)
    {
        int cost = goalNode.g;
        Node *ptr = goalNode.n;
        for(int i=cost-1; i>=0; i--)
        {
            goalSeq[i] = ptr->type;
            ptr = ptr->p;
        }
        *finalG = cost;
    }
}

string norm_move(int type)
{
    if(type == 0)
        return "UP";
    else if(type == 1)
        return "DOWN";
    else if(type == 2)
        return "LEFT";
    else if(type == 3)
        return "RIGHT";
    return "ERROR";
}

void displaySequenceOfMoves(int *moves, int n)
{
    cout << "Moves to goal state : ";
    for(int i=0; i<n; i++)
        cout << norm_move(moves[i]) << ' ';
    cout << endl;
}

int main()
{
    int t;
    cin >> t;
    while(t--)
    {
        // Init
        int moves[maxLimitF];
        int flag_h = -1, cost;
        S start_h;
        start_h.init();

        // Allocate memory and load init
        /*
        S *rootSet_d;
        int *goalSeq_d, *finalG_d, *flag_d;
        
        cudaMalloc((void **)&rootSet_d, sizeof(S));
        cudaMalloc((void **)&flag_d, sizeof(int));
        /cudaMalloc((void **)&finalG_d, sizeof(int));

        cudaMemcpy(rootSet_d, &start_h, sizeof(S), cudaMemcpyHostToDevice);
        cudaMemcpy(flag_d, &flag_h, sizeof(int), cudaMemcpyHostToDevice);
        */

        // Launch kernel
        //BPIDA<<<1, ThreadsPerBlock>>>(rootSet_d, goalSeq_d, finalG_d, flag_d);
        BPIDA(&start_h, moves, &cost, &flag_h);

        // Extract results
        /*
        cudaMemcpy(&flag_h, flag_d, sizeof(int), cudaMemcpyDeviceToHost);
        if(flag_h >= 0)
        {
            cudaMemcpy(&cost, finalG_d, sizeof(int), cudaMemcpyDeviceToHost);
            moves = new int[cost];
            cudaMemcpy(moves, goalSeq_d, sizeof(int) * cost, cudaMemcpyDeviceToHost);
        }
        */

        // Free memory
        /*
        cudaFree(rootSet_d);
        cudaFree(goalSeq_d);
        cudaFree(finalG_d);
        cudaFree(flag_d);
        */

        // Display results
        if(flag_h >= 0)
        {
            cout << "Solution found." << endl;
            cout << cost << " move(s) are required to solve the problem." << endl;
            displaySequenceOfMoves(moves, cost);
        }
        else
            cout << "Solution not found(" << flag_h << ")." << endl;
    }
    return 0;
}
