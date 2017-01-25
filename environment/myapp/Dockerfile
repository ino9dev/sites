# ベースイメージとして、ruby2.3.1を利用する
FROM ruby:2.3.1

# 本Dockerfileのメンテナ
MAINTAINER ino9dev@gmail.com

# railsアプリの名称を指定する
ENV APP_NAME myapp

# railsアプリのホームディレクトリを指定する
ENV APP_HOME /$APP_NAME

# 文字コードをUTF8に設定する
ENV LANG C.UTF-8

# ビルドツール, postgresql, nodejsをインストールする
RUN apt-get update -qq &&
    apt-get install -y build-essential \
    	    	       mysql-client \
    	    	       libpq-dev \
		       nodejs
		       --no-install-recommends

# bundllerをgemインストールする
RUN gem install bundler

# (containerの)ワークディレクトリに移動する
WORKDIR /tmp

# Gemfile / Gemfile.lockを作成する
COPY Gemfile $APP_HOME
COPY Gemfile.lock $APP_HOME

# bundllerを実行する
RUN bundle install

# アプリのホームディレクトリを作成する
RUN mkdir -p $APP_HOME

# (containerの）アプリホームディレクトリに移動する
WORKDIR  $APP_HOME
ADD . $APP_HOME


