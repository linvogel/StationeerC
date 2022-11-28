FROM ocaml/opam:opensuse-15.3-opam

USER root

# setup system repositories
RUN zypper --non-interactive update
RUN zypper --non-interactive install vim nano

# add user
RUN useradd -m dev
RUN mkdir -p /home/dev
USER dev

# setup opam
RUN opam init --disable-sandboxing -y
RUN opam install -y ocaml.4.14.0
RUN opam install -y user-setup
RUN opam install -y ocamlbuild
RUN opam install -y menhir
RUN opam install -y utop
RUN opam install -y ocaml-lsp-server
RUN opam install -y merlin

VOLUME [ "/workspace" ]

ENTRYPOINT [ "/bin/bash" ]