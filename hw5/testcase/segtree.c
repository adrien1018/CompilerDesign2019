#include <stdio.h>
// #define MAIN main

void write(int x) {
	printf("%d", x);
} 
/* void write(char *x) {
  printf("%s", x);
} */
int read() {
	int x;
	scanf("%d", &x);
	return x;
}

int st[100000 * 4];
int id, l, r, qL, qR, p, v, ans;

int query() {
  /* write("l = "); write(l); write(" r = "); write(r); write(" ql = "); write(qL); write(" qr = "); write(qR); write(" id = "); write(id); write("\n"); */
	if (qL <= l && r <= qR) {
    /* write("return1\n"); */
		return st[id];
	} else {
		int m = (l + r) / 2;
		int ans = 0;
		int l_id = id, l_l = l, l_r = r, l_ql = qL, l_qR = qR;
		if (qL <= m) {
			int q;
			id = l_id * 2;
			l = l_l;
			r = m;
			q = query();
			if (q > ans) ans = q;
		}
		if (m < qR) {
			int q;
			id = l_id * 2 + 1;
			l = m + 1;
			r = l_r;
			q = query();
			if (q > ans) ans = q;
		}
    /* write("return\n"); */
		return ans;
	}
}

void modify() {
	if (l == r) {
		st[id] = v;
	} else {
		int m = (l + r) / 2;
		int l_id = id;
		if (p <= m) {
			id = id * 2;
			r = m;
			modify();
		} else {
			id = id * 2 + 1;
			l = m + 1;
			modify();
		}
		st[l_id] = st[l_id * 2];
		if (st[l_id * 2 + 1] > st[l_id]) st[l_id] = st[l_id * 2 + 1];
	}
}

int main() {
	int i = 100, j, n = read(), q = read(), rand = 7122;
  write(n); // write("\n");
  write(q);// write("\n");
  i = 0;
	while (i < n) {
		/* write(i);
		write("\n"); */
		id = 1;
		l = 0;
		r = n - 1;
		p = i;
		v = i;
		modify();
    i = i + 1;
	}
  i = 0;
  while (i < n) {
    j = 0;
    while (j < n) {
      /* write("i = "); write(i); write(" j = "); write(j); write("\n"); */
			id = 1;
			l = 0;
			r = n - 1;
			qL = i;
			qR = j;
      query();
      /* write("done\n"); */
			ans = qR;
			if (ans != qR) {
				write(1111111);
			}
      j = j + 1;
		}
    i = i + 1;
	}
}


