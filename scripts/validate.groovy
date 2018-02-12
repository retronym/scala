#!groovy

def testJobsPaths = []
def scalaVersion = ""

stage('publish') {
    node("") {
        try {
            setDisplayName()
            scalaCheckout()
            ansiColor('xterm') {
                runScript("scripts/jobs/validate/publish-core")
            }
            def props = readProperties file: 'jenkins.properties'
            scalaVersion = props['maven.version.number']
            testJobsPaths = findFiles(glob: jobsGlob)
            sh """#!/bin/bash
                . scripts/common
                sbt -Dstarr.version=${scalaVersion} test:compile
            """
            stash name: "build", includes: "build/**/*"
        } finally {
            archiveArtifacts artifacts: 'hs_err_*.log,jenkins.properties', allowEmptyArchive: true
        }
    }
}

parallel {
    sbt("junit", "junit/test")
    sbt("pos", "partest --pos")
}

parallel testJobsPaths.collectEntries{testStage(scalaVersion, it)}

def sbt(name, args) {
    action = { ->
        node { stage(name) {
            try {
                println("Starting stage ${name} to run `sbt ${args}` on ${env.NODE_NAME}@${env.EXECUTOR_NUMBER} in ${env.WORKSPACE}")
                scalaCheckout()
                env['scalaVersion'] = scalaVersion
                unstash("build")
                ansiColor('xterm') {
                    sh """#!/bin/bash
                        . scripts/common
                        sbt -Dstarr.version=${scalaVersion} 'set antStyle := true' update "$args"
                    """
                }
            }
            finally {
                println("Ending stage ${name} to run ${scriptFile} on ${env.NODE_NAME}@${env.EXECUTOR_NUMBER} in ${env.WORKSPACE}")

                archiveArtifacts artifacts: 'hs_err_*.log,**/test-reports/**/*.xml,build/junit/TEST-*,build/osgi/TEST-*', allowEmptyArchive: true
                junit allowEmptyResults: true, testResults: '**/test-reports/**/*.xml'
            }
        }}
    }
    [name, action]
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// END OF BUILD PROPER
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


def setDisplayName() {
    currentBuild.setDisplayName("[${currentBuild.number}] $repo_user/$repo_name#$_scabot_pr at ${repo_ref.take(6)}")
}

def scalaCheckout() {
    checkout changelog: false, poll: false, scm: [$class: 'GitSCM', branches: [[name: '${repo_ref}']], doGenerateSubmoduleConfigurations: false, extensions: [[$class: 'CleanCheckout']], submoduleCfg: [], userRemoteConfigs: [[name: '"https://github.com/${repo_user}/${repo_name}.git', refspec: '+refs/heads/*:refs/remotes/${repo_user}/* +refs/pull/*/head:refs/remotes/${repo_user}/pr/*/head']]]
}

def runScript(path) {
    sh """#!/bin/bash -ex
    if [ -f /usr/local/share/jvm/jvm-select ]; then
        . /usr/local/share/jvm/jvm-select;
        jvmSelect $jvmFlavor $jvmVersion;
    else
        echo 'WARNING: jvm-select not present. using system default Java';
    fi
    echo scalaVersion=\$scalaVersion
    echo BASH_VERSION="\$BASH_VERSION"
    . ${path}
    """
}
