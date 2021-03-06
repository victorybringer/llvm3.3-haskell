From haskell:8.2.2

COPY sources.list  /etc/apt/

RUN  apt-get update &&  apt-get install  -y aptitude && aptitude search llvm &&  apt-get install -y llvm-3.3 &&   apt-get install -y libgmp-dev &&   apt-get install -y g++ && mkdir /root/haskell && mkdir /root/LLVM



RUN apt-get update && \
    apt-get install -y --no-install-recommends  \
        clang-3.3 llvm-3.3 llvm-3.3-dev llvm-3.3-runtime libllvm3.3\
   #     cabal-install ghc happy alex \
   #     zlib1g-dev libtinfo-dev libsqlite3-0 libsqlite3-dev ca-certificates g++ git curl \
        graphviz g++ python3  && \
    ln -sT /usr/bin/opt-3.3 /usr/local/bin/opt 




COPY config /root/.cabal/

RUN cabal update && cabal install c2hs 



COPY LLVM   /root/LLVM



RUN cd /root/LLVM/llvm-base-types-0.3 && cabal install 


RUN cd /root/LLVM/llvm-data-interop-0.3 && cabal install  


RUN cd /root/LLVM/llvm-analysis-0.3 && cabal install 



RUN cd /root/LLVM/llvm-tools-0.2.0.1 && cabal install 

RUN cd /root/LLVM/llvm-slicing-0.3 && cabal install 


COPY testfile.c     /root/.cabal/bin


COPY testfile.c     /root/LLVM/llvm-slicing-0.3


WORKDIR  /root/.cabal/bin


RUN cd /root/.cabal/bin && clang testfile.c -emit-llvm  -c -g -o testfile.bc


CMD ["./DumpLLVMModule", "testfile.bc"]





