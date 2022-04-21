#include <stdlib.h>
#include <benchmark/benchmark.h>
extern "C" void p(long*,long*,long*);
extern "C" void f(long*,long*,long*);
extern "C" void g(long*,long*,long*);

// Define another benchmark
static void BM_GCC(benchmark::State& state) {
  long* A = (long*)calloc(100, sizeof(long));
  long* B = (long*)calloc(100, sizeof(long));
  long* C = (long*)calloc(100, sizeof(long));
  
  for (auto _ : state) {
  	f(A,B,C);
  }
}

BENCHMARK(BM_GCC);

static void BM_JASMIN(benchmark::State& state) {
  long* A = (long*)calloc(100, sizeof(long));
  long* B = (long*)calloc(100, sizeof(long));
  long* C = (long*)calloc(100, sizeof(long));
  
  for (auto _ : state) {
	p(A,B,C);
  }
}
// Register the function as a benchmark
BENCHMARK(BM_JASMIN);

static void BM_CLANG(benchmark::State& state) {
  long* A = (long*)calloc(100, sizeof(long));
  long* B = (long*)calloc(100, sizeof(long));
  long* C = (long*)calloc(100, sizeof(long));
  
  for (auto _ : state) {
	g(A,B,C);
  }
}
// Register the function as a benchmark
BENCHMARK(BM_CLANG);


// Run the benchmark
BENCHMARK_MAIN();

