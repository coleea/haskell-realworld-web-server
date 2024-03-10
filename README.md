# Example of Realworld-Haskell Web Server 

## Stack

- Stack: Haskell의 빌드 도구
- Server : [Scotty](https://hackage.haskell.org/package/scotty) - inspired by Ruby's Sinatra, using WAI and Warp
- DB : Postgresql
- Aeson: JSON 데이터를 파싱하고 생성하는 데 사용되는 Haskell 라이브러리입니다. 관련 코드는 Aeson.hs에서 찾을 수 있습니다.
- JWT (Json Web Tokens): 사용자 인증에 사용되는 토큰 형식입니다. 관련 코드는 JwtTokenGateway.hs에서 찾을 수 있습니다.
- HTTP Server: 웹 서버 구성요소입니다. 관련 코드는 HttpServer.hs에서 찾을 수 있습니다.





## Instruction (MacOS)

```
brew install postgresql
brew install pcre
```

이후 postgresql를 실행합니다. 로컬에서 실행하거나 Supabase등의 DBaaS를 사용해도 좋습니다

- 환경변수 설정. 프로덕션 실행을 위해서는 아래의 환경변수를 필요로 합니다
    - DB_HOST
    - DB_USER
    - DB_PORT
    - DB_PASSWORD
    - DB_DATABASE
    - DB_POOL_MAX_SIZE
    - DB_POOL_IDLE_TIMEOUT_SEC

환경변수 설정을 위해 터미널에서 다음과 같이 입력합니다 (예시)
```
export DB_PORT=5432
```



GHCup을 설치합니다
```
https://www.haskell.org/ghcup/
```


의존성을 설치하기 위해 다음 명령을 실행합니다

```sh
stack setup
```

빌드
```sh
stack build
```


프로그램을 프로덕션 모드로 실행하려면 다음 명령을 사용합니다

```sh
stack exec realworld-exe
```

개발 환경에서 실행하려면 다음과 같이 실행합니다:
(개발 환경으로 구동하려면 Postgresql이 로컬에서 실행되어야 합니다)
```sh
stack exec dev
```


테스트를 실행하려면 다음 명령을 사용합니다:

```sh
stack test
```