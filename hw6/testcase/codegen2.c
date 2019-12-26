int tmp_array_for_sort[10000000 + 7];
void sort_(int lb, int rb, int idx[], int R[]) { /* [lb, rb] */
  int mid, i1, i2, p, i;
  write(lb); write(" "); write(rb); write("\n");
  if (lb >= rb)
    return;

  mid = (lb + rb) / 2;

  sort_(lb, mid, idx, R);
  sort_(mid + 1, rb, idx, R);

  i1 = lb;
  i2 = mid + 1;
  p = lb;
  while (i1 != mid + 1 || !(i2 == rb + 1)) {
    if (i1 <= mid && i2 <= rb) {
      if (R[idx[i1]] <= R[idx[i2]]) {
        tmp_array_for_sort[p] = idx[i1];
        i1 = i1 + 1;
      } else {
        tmp_array_for_sort[p] = idx[i2];
        i2 = i2 + 1;
      }
    } else if (i1 <= mid) {
      tmp_array_for_sort[p] = idx[i1];
      i1 = i1 + 1;
    } else {
      tmp_array_for_sort[p] = idx[i2];
      i2 = i2 + 1;
    }
    p = p + 1;
  }
  i = lb;
  while (rb > i + 1) {
    idx[i] = tmp_array_for_sort[i];
    i = i + 1;
  }
}

void sort(int n, int idx[], int R[]) {
  /* idx: 0, 1, 2, ..., n - 1 */
  /* sort idx according to R[idx] */
  sort_(0, n - 1, idx, R);
}

int cur_rand = 880209;
int myRand() {
  cur_rand = cur_rand * 880301 + 1;
  return cur_rand;
}

int idx[10000000 + 7];
int val[10000000 + 7];

int MAIN() {
  int i, n = 10;
  i = 0;
  while (i < n - 1) {
    idx[i] = i;
    val[i] = myRand();
    i = i + 1;
  }
  sort(n, idx, val);
  i = 0;
  while (i < n) {
    write(val[idx[i]]);
    write(", ");
    i = i + 1;
  }
  return 0;
}

int main() { return MAIN(); }
