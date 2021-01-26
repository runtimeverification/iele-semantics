pipeline {
  agent { label 'docker' }
  environment {
    KIELE_VERSION     = '0.1.0'
    GITHUB_TOKEN      = credentials('rv-jenkins')
    SHORT_REV         = """${sh(returnStdout: true, script: 'git rev-parse --short HEAD').trim()}"""
    LONG_REV          = """${sh(returnStdout: true, script: 'git rev-parse HEAD').trim()}"""
    KIELE_RELEASE_TAG = "v${env.KIELE_VERSION}-${env.SHORT_REV}"
    K_SHORT_REV       = """${sh(returnStdout: true, script: 'cat deps/k_release | cut --delimiter="-" --field="2"').trim()}"""
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
        stage('Build')               { steps { sh 'make build       -j4'        } }
        stage('Split Tests')         { steps { sh 'make split-tests -j4'        } }
        stage('Assemble Iele Tests') { steps { sh 'make assemble-iele-test -j4' } }
        stage('Test') {
          options { timeout(time: 30, unit: 'MINUTES') }
          parallel {
            stage('EVM Tests')            { steps { sh 'make test-vm -j4'                        } }
            stage('IELE Tests')           { steps { sh 'make test-iele -j4'                      } }
            stage('IELE Tests (Haskell)') { steps { sh 'make test-iele -j4 TEST_BACKEND=haskell' } }
            stage('Well Formed Check')    { steps { sh 'make test-wellformed -j4'                } }
            stage('Ill Formed Check')     { steps { sh 'make test-illformed -j4'                 } }
            stage('Interactive')          { steps { sh 'make test-interactive'                   } }
            stage('Node') {
              steps {
                sh '''
                  export PATH=$(pwd)/.build/bin:$PATH
                  kiele vm --port 9001 &
                  pid=$!
                  sleep 3
                  make test-iele-node  -j4 TEST_PORT=9001
                  make test-bad-packet -j4 TEST_PORT=9001
                  kill $pid
                '''
              }
            }
          }
        }
      }
    }
    stage('Package') {
      stages {
        stage('Checkout SCM') { steps { dir("kiele-${KIELE_VERSION}-src") { checkout scm } } }
        stage('Binary Package') {
          when {
            branch 'master'
            beforeAgent true
          }
          agent {
            dockerfile {
              reuseNode true
              additionalBuildArgs '--build-arg K_COMMIT=${K_SHORT_REV} --build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g)'
            }
          }
          steps {
            sh '''
              make install INSTALL_PREFIX=$(pwd)/kiele-${KIELE_VERSION}-bin
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
                  sh '../kiele-${KIELE_VERSION}-bionic/package/debian/test-package.sh ${KIELE_VERSION} bionic ${LONG_REV} 9001'
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
      agent {
        dockerfile {
          label 'docker'
          additionalBuildArgs '--build-arg K_COMMIT=$(cat deps/k_release | cut --delimiter="-" --field="2") --build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g)'
        }
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
        stage('Update Dependents') {
          steps {
            build job: 'rv-devops/master', propagate: false, wait: false                                                   \
                , parameters: [ booleanParam ( name: 'UPDATE_DEPS'         , value: true                                 ) \
                              , string       ( name: 'UPDATE_DEPS_REPO'    , value: 'runtimeverification/iele-semantics' ) \
                              , string       ( name: 'UPDATE_DEPS_VERSION' , value: "${env.KIELE_RELEASE_TAG}"           ) \
                              ]
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
                  git submodule update --init --recursive -- ./web
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
