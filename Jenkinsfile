pipeline {
  agent { label 'docker' }
  environment {
    KIELE_VERSION     = '0.1.0'
    GITHUB_TOKEN      = credentials('rv-jenkins')
    SHORT_REV         = """${sh(returnStdout: true, script: 'git rev-parse --short HEAD').trim()}"""
    LONG_REV          = """${sh(returnStdout: true, script: 'git rev-parse HEAD').trim()}"""
    KIELE_RELEASE_TAG = "v${env.KIELE_VERSION}-${env.SHORT_REV}"
    K_SHORT_REV       = """${sh(returnStdout: true, script: 'cat deps/k_release').trim()}"""
  }
  options { ansiColor('xterm') }
  stages {
    stage("Init title") {
      when { changeRequest() }
      steps { script { currentBuild.displayName = "PR ${env.CHANGE_ID}: ${env.CHANGE_TITLE}" } }
    }
    stage('Build and Test') {
      agent {
        dockerfile {
          label 'docker'
          additionalBuildArgs '--build-arg K_COMMIT=$(cat deps/k_release | cut --delimiter="-" --field="2") --build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g)'
        }
      }
      stages {
        stage('Build') { steps { sh 'make COVERAGE=k -j4' } }
        stage('Test') {
          stages {
            stage('VM Tests') {
              options { timeout(time: 5, unit: 'MINUTES') }
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
              options { timeout(time: 5, unit: 'MINUTES') }
              failFast true
              steps { sh 'make -j2 iele-test-haskell' }
            }
          }
        }
      }
    }
    stage('Package') {
      stages {
        stage('Checkout SCM') { steps { dir("kiele-${KIELE_VERSION}-src") { checkout scm } } }
        stage('Binary Package') {
          // when {
          //   branch 'master'
          //   beforeAgent true
          // }
          agent {
            dockerfile {
              reuseNode true
              additionalBuildArgs '--build-arg K_COMMIT=${K_SHORT_REV} --build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g)'
            }
          }
          steps {
            sh '''
              make install INSTALL_PREFIX=$(pwd)/kiele-${KIELE_VERSION}-bin
              cp install.sh kiele-${KIELE_VERSION}-bin/
              tar czvf kiele-${KIELE_VERSION}-bin.tar.gz kiele-${KIELE_VERSION}-bin
            '''
            stash name: 'bin-kiele', includes: "kiele-${env.KIELE_VERSION}-bin.tar.gz"
          }
        }
        stage('Ubuntu Bionic') {
          stages {
            stage('Build Package') {
              agent {
                dockerfile {
                  dir "kiele-${env.KIELE_VERSION}-src/package/debian"
                  additionalBuildArgs '--build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g) --build-arg BASE_IMAGE=ubuntu:bionic'
                  reuseNode true
                }
              }
              steps {
                dir("kiele-${env.KIELE_VERSION}-bionic") {
                  checkout scm
                  sh './package/debian/build-package.sh ${K_SHORT_REV} bionic'
                  stash name: 'bionic-kframework', includes: "kframework-bionic.deb"
                }
                stash name: 'bionic-kiele', includes: "kiele_${env.KIELE_VERSION}_amd64_bionic.deb"
              }
            }
            stage('Test Package') {
              agent {
                dockerfile {
                  filename "kiele-${env.KIELE_VERSION}-src/package/debian/Dockerfile.test"
                  additionalBuildArgs '--build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g) --build-arg BASE_IMAGE=ubuntu:bionic'
                  reuseNode true
                }
              }
              options { timeout(time: 15, unit: 'MINUTES') }
              steps {
                dir("kiele-${env.KIELE_VERSION}-bionic-test") {
                  unstash 'bionic-kiele'
                  sh '../kiele-${KIELE_VERSION}-bionic/package/debian/test-package.sh ${KIELE_VERSION} bionic'
                }
              }
            }
          }
        }
      }
    }
    stage('Deploy') {
      when {
        branch 'master'
        beforeAgent true
      }
      post { failure { slackSend color: '#cb2431' , channel: '#iele-internal' , message: "Deploy Phase Failed: ${env.BUILD_URL}" } }
      stages {
        stage('GitHub Release') {
          steps {
            unstash 'bin-kiele'
            unstash 'bionic-kiele'
            sshagent(['2b3d8d6b-0855-4b59-864a-6b3ddf9c9d1a']) {
              sh '''
                git clone 'ssh://github.com/runtimeverification/iele-semantics.git' kiele-release
                cd kiele-release
                git fetch --all

                git tag -d "${KIELE_RELEASE_TAG}" "${SHORT_REV}" || true
                git push -d origin "${KIELE_RELEASE_TAG}"        || true
                hub release delete "${KIELE_RELEASE_TAG}"        || true

                git tag "${KIELE_RELEASE_TAG}" "${LONG_REV}"
                git push origin "${KIELE_RELEASE_TAG}"

                make release.md KIELE_VERSION=${KIELE_RELEASE_TAG}
                hub release create                                                                      \
                    --attach "../kiele-${KIELE_VERSION}-bin.tar.gz#KIELE Linux Binary"                  \
                    --attach "../kiele_${KIELE_VERSION}_amd64_bionic.deb#Ubuntu Bionic (18.04) Package" \
                    --file "release.md" "${KIELE_RELEASE_TAG}"
              '''
            }
          }
        }
        stage('GitHub Pages') {
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
                  npm run build-sitemap
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
  }
}
