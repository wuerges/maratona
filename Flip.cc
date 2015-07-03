#include <iostream>
#include <deque>
using namespace std;




int mask[] = 
    { 0xC800   
    , 0xE400
    , 0x7200
    , 0x3100
    , 0x8C80
    , 0x4E40
    , 0x2720
    , 0x1310
    , 0x08C8
    , 0x04E4
    , 0x0272
    , 0x0131
    , 0x008C
    , 0x004E
    , 0x0027
    , 0x0013
    };

int flip(int i, int p) {
    return i ^ mask[p];
}

int cache[2 << 16];

int solve(int t) {
    deque<int> todo;
    todo.push_back(0x0000);
    todo.push_back(0xffff);

    int i = 0;
    while(todo.size() > 0) {
        ++i;
        int w = todo.front();
        todo.pop_front();

        for (int i = 0; i < 16; ++i) {
           int f = flip(w, i);

           if(cache[f] == -1) {
               cache[f] = cache[w] + 1;
               todo.push_back(f);
           }
           else if (cache[f] > cache[w] + 1) {
               cache[f] = cache[w] + 1;
           }
        }

        if (cache[t] >= 0) {
            // cout << dec << "DEBUG iterations: " << i << endl;
            return cache[t];
        }
    }
    
    // cout << dec << "DEBUG iterations: " << i << endl;
    return -1;
}

int read_input() {
    string l;
    int t = 0;
    for (int i = 0; i < 4; ++i) {
        cin >> l;
        for(string::iterator ji = l.begin(), je = l.end(); ji != je; ++ji) {
            if (*ji == 'b') {
                t |= 1;
            }
            t <<= 1;
        }
    }
    t >>= 1;
    return t;
}

int main() {
    fill_n(cache, 2 << 16, -1);
    cache[0] = 0;
    cache[0xffff] = 0;
    int t = read_input();

    // cout << hex << "DEBUG: " << t << endl;

    int s = solve(t);
    if (s  < 0 ) {
        cout << "Impossible" << endl;
    }
    else {
        cout << s << endl;
    }
}

