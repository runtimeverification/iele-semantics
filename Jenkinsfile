pipeline {
  agent {
    dockerfile {
      additionalBuildArgs '--build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g)'
    }
  }
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
        ansiColor('xterm') {
          sh '''
            eval $(opam config env)
            make deps
            make COVERAGE=k
          '''
        }
      }
    }
    stage('Test') {
      steps {
        ansiColor('xterm') {
          sh '''
            eval $(opam config env)
            .build/vm/iele-vm 0 127.0.0.1 > port &
            sleep 3
            export PORT=`cat port | awk -F ':' '{print $3}'`
            make test -j`nproc`
            make coverage
            kill %1
          '''
        }
      }
    }
  }
}
