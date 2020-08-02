# FROM ubuntu:trusty 
FROM haskell:8.2.1

MAINTAINER Zhang Yingzhou <zhangyz@njupt.edu.cn>

ENV LANG   C.UTF-8

## 最好修改apt-get成国内镜像源，详情参考： https://mirrors.tuna.tsinghua.edu.cn/help/ubuntu/
# RUN  echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' > /etc/apt/sources.list.d/ghc.list && \
#      apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 
RUN apt-get update && \
    apt-get install -y --no-install-recommends  \
        clang-3.3 llvm-3.3 llvm-3.3-dev llvm-3.3-runtime libllvm3.3\
   #     cabal-install ghc happy alex \
   #     zlib1g-dev libtinfo-dev libsqlite3-0 libsqlite3-dev ca-certificates g++ git curl \
        graphviz g++ python3  && \
    ln -sT /usr/bin/opt-3.3 /usr/local/bin/opt 
  #  pip install wllvm 

ENV HOME /root
# ENV PATH /root/.cabal/bin:/opt/cabal/bin:/opt/ghc/8.2.1/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.7/bin:$PATH


 ##  最好修改hackage国内镜像网址(配置文件在 ~/.cabal/config):  http://mirrors.tuna.tsinghua.edu.cn/hackage
RUN cabal update && \
    cabal install c2hs 

WORKDIR /

CMD ["/bin/bash"]


