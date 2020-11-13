pipeline {
  agent {
    dockerfile {
      label 'docker'
      additionalBuildArgs '--build-arg K_COMMIT=$(cat deps/k_release | cut --delimiter="-" --field="2") --build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g)'
    }
  }
  options { ansiColor('xterm') }
  stages {
    stage("Init title") {
      when { changeRequest() }
      steps {
        script {
          currentBuild.displayName = "PR ${env.CHANGE_ID}: ${env.CHANGE_TITLE}"
        }
      }
    }
    stage('Build') {
      steps {
        sh '''
          make deps
          make COVERAGE=k
        '''
      }
    }
    stage('Test') {
      stages {
        stage('VM Tests') {
          steps {
            ansiColor('xterm') {
              sh '''#!/bin/bash -ex
                .build/vm/iele-vm 0 127.0.0.1 > port &
                sleep 3
                export PORT=`cat port | awk -F ':' '{print $2}'`
                make test -j`nproc` -k
                make coverage
                kill %1
              '''
            }
          }
        }
        stage('Haskell Standalone') {
          options { timeout(time: 20, unit: 'MINUTES') }
          failFast true
          steps { sh 'make -j2 iele-test-haskell' }
        }
      }
    }
    stage('Deploy') {
      when {
        branch 'master'
        beforeAgent true
      }
      agent {
        dockerfile {
          additionalBuildArgs '--build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g)'
          reuseNode true
        }
      }
      post {
        failure {
          slackSend color: '#cb2431'                                 \
                  , channel: '#iele-internal'                                    \
                  , message: "Deploy Phase Failed: ${env.BUILD_URL}"
        }
      }
      environment {
        AWS_ACCESS_KEY_ID     = credentials('aws-access-key-id')
        AWS_SECRET_ACCESS_KEY = credentials('aws-secret-access-key')
        AWS_REGION            = 'us-east-2'
        GITHUB_TOKEN          = credentials('rv-jenkins')
        GIT_SSH_COMMAND       = 'ssh -o StrictHostKeyChecking=accept-new'
      }
      steps {
        dir('gh-pages') {
          sshagent(['2b3d8d6b-0855-4b59-864a-6b3ddf9c9d1a']) {
            sh '''
              git clone 'ssh://github.com/runtimeverification/iele-semantics.git' --depth 1 --no-single-branch --branch master --branch gh-pages
              cd iele-semantics
              git checkout -B gh-pages origin/master
              cd web
              npm install
              npm run build
              cd -
              mv web/public_content ./
              rm -rf $(find . -maxdepth 1 -not -name public_content -a -not -name .git -a -not -path . -a -not -path .. -a -not -name CNAME)
              mv public_content/* ./
              rm -rf public_content
              git add ./
              git commit -m 'gh-pages: Updated the website'
              git merge --strategy ours origin/gh-pages --allow-unrelated-histories
              git push origin gh-pages
            '''
          }
        }
      }
    }
  }
}
