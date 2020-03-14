FROM ocaml/opam2:4.09
RUN sudo apt-get install -y m4 git net-tools iproute2 libev-dev qemu-kvm
RUN opam update -y
RUN opam install -y conf-libev mirage
RUN mkdir /home/opam/blog.x25519.net
RUN git clone https://github.com/dinosaure/blog.x25519.net.git /home/opam/blog.x25519.net
WORKDIR /home/opam/blog.x25519.net/unikernel
RUN opam config exec -- mirage configure -t hvt
RUN make depends
RUN opam config exec -- mirage build
COPY tap.sh /home/opam/blog.x25519.net/unikernel/tap.sh
EXPOSE 80
ENV VIRTUAL_HOST blog.x25519.net
ENV LETSENCRYPT_HOST blog.x25519.net
COPY run.sh /home/opam/blog.x25519.net/unikernel/run.sh
