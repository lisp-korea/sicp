Structure and Instruction of Computer Programming Examples
=======================================


Description
----------
[Lisp을 좋아하는 사람들의 그룹][lispkorea]들에서 진행중인 [Structure and Instruction of
Computer Programming][SICP](이하 SICP) 스터디의 연습문제 풀이를 공유하기 위한 저장소입니다.

Requirements
-----------
 * SICP 교재
 * 실습에 사용할 플랫폼(공식적으론 Scheme)
 
How to
------
 1. [github][github] 가입
    * 첫 화면에서 아래 Plans, Pricing and Signup 버튼 클릭
    * Plan & Pricing 화면에서 $0/mo Free for open source 란 Create a free account 버튼 클릭
    * user name, email, password를 등록합니다. 
 2. lisp-korea/sicp repo 접근권한 요청
    * 게시판에 등록한 github user name을 올려주세요.
    * 지난시간에 나오셔서 스터디 참석의사를 보여주신 분은 바로
      등록해드리겠습니다. (30분~1시간정도 소요 예정) 
 3. SSH Public Key 등록
    * 각 OS별 public key를 생성하세요
    * http://help.github.com/ 에 가시면 오른쪽 column 링크에 각 OS
	public key 생성방법이 나옵니다.
    * 생성된 key를 가신의 계정 Account Settings의 SSH Public Keys ->
       Add another public key에 등록합니다. 
 4. lisp-korea/sicp repo 가져오기
    * git clone git@github.com:lisp-korea/sicp.git
 5. git user 설정
    * git config --global user.name "user name" (github등록한 거요)
    * git config --global user.email "email 주소"
 6. 개인 소스 올리기
    * cd ch01
    * ex-1-1-<id>.lisp or ex-1-1-<id>.clj 식의 naming으로 소스 생성
    * git add <filename>
    * git commit -m "ex 1.1 by 누구누구"
    * git push origin master6. 개인 소스 올리기
    * cd ch01
    * ex-1-1-<id>.lisp or ex-1-1-<id>.clj 식의 naming으로 소스 생성
    * git add <filename>
    * git commit -m "ex 1.1 by 누구누구"
    * git push origin master       
      
[SICP]: http://mitpress.mit.edu/sicp/
[lispkorea]: http://groups.google.com/group/lisp-korea
[github]:http://github.com
