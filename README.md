# Compiler Optimization & Construction

> C 계열 소스 코드를 **3-주소 IR로 변환**하고, **데이터플로우 분석(DFA)** 기반으로 저수준 최적화를 수행하는 컴파일러 **미들엔드(Middle-end)** 구현 프로젝트.

소스 → IR → CFG 위에서의 정적 분석 → 최적화 패스의 반복 적용까지, 실제 최적화 컴파일러의 핵심 파이프라인을 직접 구현하고 **명령어 비용 모델로 효과를 정량 측정**했다.

---

## 🔧 Pipeline

```
소스코드 (.minc)
   │  Lexer (FsLex) → Parser (FsYacc)
   ▼
  AST                         [AST.fs]
   │  Translate (lowering)
   ▼
3-Address IR                  [Translate.fs, IR.fs]
   │  CFG.make (명령어 단위 제어흐름그래프)
   ▼
  CFG ──► Data Flow Analysis  [CFG.fs, DFA.fs]
   │        · Reaching Definitions (forward, ∪)
   │        · Liveness          (backward, ∪)
   ▼
Optimization (고정점까지 반복) [Optimize.fs]
   │        · Constant Folding / Propagation
   │        · Mem2Reg
   ▼
최적화된 IR ──► Executor (IR 인터프리터 + 비용 측정)  [Executor.fs]
```

---

## 🧩 핵심 구현

### 1. IR Translation & Control Flow  `Translate.fs` · `CFG.fs`
- AST를 **3-주소 코드(3-Address Code)** 형태의 IR로 lowering. 모든 변수를 일단 메모리에 할당(`LocalAlloc`/`Load`/`Store`)하는 단순 모델로 시작.
- `&&` / `||`는 `BinOp`이 아니라 **단락 평가(short-circuit)** 제어 흐름으로 번역해 C 의미론을 보존.
- 명령어 1개를 노드 1개로 갖는 **CFG**를 구성 (successor / predecessor 맵).

### 2. Data Flow Analysis Engine  `DFA.fs`
**고정점 반복(fixpoint iteration)** 으로 두 분석을 구현:

| 분석 | 방향 | meet | transfer | 용도 |
|---|---|---|---|---|
| **Reaching Definitions** | forward | ∪ (may) | `OUT = gen ∪ (IN − kill)` | Constant Propagation |
| **Liveness** | backward | ∪ (may) | `IN = use ∪ (OUT − def)` | Dead Code Elimination |

> 종료 보장: transfer 함수가 단조(monotone)하고 격자(lattice) 높이가 유한하므로 유한 단계에 수렴.

### 3. Middle-end Optimization  `Optimize.fs`
- **Mem2Reg** — 스택 메모리 접근(`Load`/`Store`, 비용 5)을 레지스터 연산(`Set`, 비용 1)으로 승격 → **메모리 I/O 비용 최소화** (효과가 가장 큰 패스).
- **Constant Folding** — 컴파일 타임에 계산 가능한 식을 상수로 축약 (`200 * 2` → `400`).
- **Constant Propagation** — RD 분석을 근거로, **모든 도달 정의가 동일 상수일 때만** 피연산자를 상수로 치환 (may-분석의 건전성 조건 준수).
- **Dead Code Elimination** — Liveness 분석을 근거로, 정의한 레지스터가 **live-OUT에 없는** 명령(죽은 계산)을 제거.
- **Iterative Optimization Loop** — 각 패스가 서로 새로운 최적화 기회를 만들기 때문에, **변화가 없을 때까지 반복**(`optimizeLoop`)하여 수렴시킴.

---

## 📊 측정 결과 (Cost Model)

`Load/Store = 5`, `BinOp = 3`, `Set = 1` 등 명령어별 비용을 합산해 최적화 전/후를 비교.

| 테스트 | 최적화 전 | 최적화 후 | 절감 |
|---|---|---|---|
| 직선 코드 (상수 연산 위주) | Cost **78** | Cost **9** | **≈ 88% ↓** |
| 루프 코드 (`while`, n=5) | Cost **353** | Cost **131** | **≈ 63% ↓** |

> 결과값은 최적화 전후가 동일(의미 보존)하면서 비용만 감소.

---

## 💻 Tech Stack

- **Language**: F# (Functional Programming)
- **Tools**: FsLex, FsYacc (Lexer & Parser Generator)
- **Concepts**: Static Analysis, Data Flow Analysis, IR Optimization, Control Flow Graph, Fixpoint Iteration
