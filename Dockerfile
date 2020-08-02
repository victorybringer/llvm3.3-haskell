
FROM haskell:8.2.1


COPY sources.list  /etc/apt/

ENV LANG   C.UTF-8


RUN apt-get update && \
    apt-get install -y --no-install-recommends  \
        clang-3.3 llvm-3.3 llvm-3.3-dev llvm-3.3-runtime libllvm3.3\
   
        graphviz g++ python3  && \
    ln -sT /usr/bin/opt-3.3 /usr/local/bin/opt  &&mkdir /root/LLVM
 

ENV HOME /root

COPY config /root/.cabal/

RUN cabal update && \
    cabal install c2hs 

COPY LLVM   /root/LLVM

WORKDIR /root/LLVM

CMD ["/bin/bash"]



