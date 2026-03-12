# Compiler Optimization & Construction
C-style 소스 코드 분석 -> IR로 변환 -> DFA 기반 저수준 최적화 구현(Middle-end)

## Data Flow Analysis Engine (DFA.fs)
- Reaching Definition Analysis: 각 프로그램 포인트에서 유효한 변수 정의 추적 &rarr; **Constant Propagtaion** 기반 마련
- Liveness Analysis: 변수의 생존 범위 정적 분석 &rarr; 불필요한 연산 제거(DCE), 리소스 관리 효율성 향상

## Middle-end Optimization(Optimize.fs)
### Mem2Reg(Memory to Register)
- 스택 기반 메모리 접근을 레지스터 기반 연산으로 승격 &rarr; **Memory I/O 비용 최소화**
### Constant Folding & Propagtion
- 컴파일 타임에 계산 가능한 로직 미리 처리 &rarr; 런타임 오버헤드 감소
### Iterative Optimization Loop
- 각 최적화 단계가 서로 영향을 주며 수렴할 때까지 반복 수행하는 `optimizeLoop` 구현

## IR Translation & Control Flow(`Translate.fs`, `CFG.fs`)
- AST 기반 3-Address Code 형태의 IR 생성 &rarr; CFG(제어 흐름 그래프)로 구조화

## 💻 Tech Stack
- Language: F# (Functional Programming)
- Tools: FsLex, FsYacc (Lexer & Parser Generator)
- Concepts: Static Analysis, DFA, IR Optimization, System Programming
