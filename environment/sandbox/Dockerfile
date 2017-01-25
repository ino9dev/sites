# ベースイメージとして、ubuntu:16.04を利用する
FROM ubuntu:xenial

# 本Dockerfileのメンテナ
MAINTAINER ino9dev <ino9dev@gmail.com>

# ダウンロード先にjpを追加

# ユーザを作成
RUN useradd -ms /bin/bash user

# 本コンテナのホームディレクトリを設定する
# ENV CONTAINER_HOME sandbox

# 文字コードをUTF8に設定する
ENV LANG C.UTF-8

# インタラクティブにしない
env DEBIAN_FRONTEND noninteractive

# wgetとsudoをインストールする
RUN apt-get update && \
    apt-get install -y wget \
    	    	       sudo

# 構成管理をインストールする
RUN apt-get update -qq && \
    apt-get install -y git

# プログラミング言語をインストールする
RUN apt-get update -qq && \
    apt-get install -y ruby2.3 \
    	    	       python3-dev \
		       openjdk-8-jdk \
		       clisp

# AI & 人工知能イメージをインストールする
# TensorFlowについては以下のようにして起動可能だった
# docker run -it -p 8888:8888 gcr.io/tensorflow/tensorflow でイメージダウンロード可能だった

# 負荷試験ツールをインストールする
RUN mkdir -p /usr/loca/sbin/jmeter3.0
WORKDIR /usr/local/sbin/jmeter3.0
RUN wget -c http://ftp.ps.pl/pub/apache//jmeter/binaries/apache-jmeter-3.0.tgz
RUN tar xvzf apache-jmeter-3.0.tgz

# セキュアサーバをインストールする
RUN apt-get update -qq && \
    apt-get install -y openssh-server

# ブラウザをインストールする
RUN apt-get update -qq && \
    apt-get install -y chromium-browser

# 実験数学用ツールをインストールする
RUN apt-get update -qq && \
    apt-get install -y maxima \
    	    	       wxmaxima \
		       gnuplot5 \
		       scilab \
		       r-base

# エディタをインストールする
RUN apt-get update -qq && \
    apt-get install -y emacs24-nox \
    	    	       emacs-mozc \
		       vim

# CIをインストールする
# jenkins は 8080で動作
RUN wget -q -O - http://pkg.jenkins-ci.org/debian/jenkins-ci.org.key | apt-key add -
RUN echo deb http://pkg.jenkins-ci.org/debian binary/ > /etc/apt/sources.list.d/jenkins.list
RUN apt-get update -qq && \
    apt-get install -y jenkins

# ドキュメントシステムをインストールする
RUN apt-get update -qq && \
    apt-get install -y texlive-luatex \
    	    	       pandoc

# root パスワードを変更しておく（以下のパスワード部分はbuild前に変更）
RUN echo "root:password" | chpasswd

# dockerユーザを作成して、sudo グループに含める
RUN useradd -d /-ms /bin/bash docker && echo "docker:docker" | chpasswd && adduser docker sudo

# dockerユーザになる
USER docker

# ユーザのパスワードも変更しておく（以下のパスワード部分はbuild前に変更）
RUN echo "user:password" | chpasswd

USER root
