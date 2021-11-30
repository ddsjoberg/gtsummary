pipeline {
  agent any {
    stages {
      stage('Build') {
        steps {
          make build
        }
      }
      stage('Check') {
        steps {
          make check
        }
      }
      stage('Clean') {
        steps {
          make clean
        }
      }
    }
  }
}
