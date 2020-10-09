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
      steps {
        ansiColor('xterm') {
          sh '''#!/bin/bash
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
