#/bin/bash
echo ">>> try to configure nerlo build"

SCRIPT_DIR="`dirname \"${0}\"`"
SCRIPT_DIR="`( cd \"${SCRIPT_DIR}\" && pwd )`"

JAVA_DIST=$SCRIPT_DIR/java/dist
JAVA_LIB=$SCRIPT_DIR/java/lib
ERL_LIB=$SCRIPT_DIR/erl/priv

if [ -f conf.cache ]; then
    echo "reading some vars from conf.cache"
    . conf.cache
else
    touch conf.cache
fi

# ERL_HOME
if [ -n "${ERL_HOME+x}" -a -x ${ERL_HOME}/bin/erlc ]; then
    echo "using ERL_HOME=$ERL_HOME"
else 
    echo "ERL_HOME not set"
    echo "please, enter path to your Erlang distribution root directory:"
    read -p '> ' ERL_HOME
    if [ -x ${ERL_HOME}/bin/erlc ]; then
        echo "using ERL_HOME=$ERL_HOME"
        echo "export ERL_HOME=$ERL_HOME" >> conf.cache
    else
        echo "ERROR: '$ERL_HOME' seems not to be an Erlang distribution"
        exit 1
    fi
fi    
export ERL_HOME

# JAVA_HOME
if [ -n "${JAVA_HOME+x}" -a -x ${JAVA_HOME}/bin/javac ]; then
    echo "using JAVA_HOME=$JAVA_HOME"
else 
    echo "JAVA_HOME not set"
    echo "please, enter path to your JDK rootls  directory:"
    read -p '> ' JAVA_HOME
    if [ -x ${JAVA_HOME}/bin/javac ]; then
        echo "using JAVA_HOME=$JAVA_HOME"
        echo "export JAVA_HOME=$JAVA_HOME" >> conf.cache
    else
        echo "ERROR: '$JAVA_HOME' seems not to be a JDK"
        exit 1
    fi
fi
export JAVA_HOME


echo "setup path in Erlang source"
BIN_SETUP=erl/bin/setup.inc
test -f $BIN_SETUP && rm $BIN_SETUP
touch $BIN_SETUP
echo "ERL_HOME=$ERL_HOME" >> $BIN_SETUP
echo "JAVA_HOME=$JAVA_HOME" >> $BIN_SETUP
PATHS_MK=erl/include/paths.mk
touch $PATHS_MK
test -f $PATHS_MK && rm $PATHS_MK
echo "ERL_HOME=$ERL_HOME" >> $PATHS_MK
echo "JAVA_HOME=$JAVA_HOME" >> $PATHS_MK


# ANT
ANT=`which ant`
if [ $? -gt 0 ]; then
    echo "ERROR: Ant not found in PATH"
    echo "------ please install Ant and add to PATH"
    exit 1
fi
echo "found Ant at $ANT"
#export ANT

echo "checking Java dependencies"
# OtpErlang.jar
OTPERLANGJAR=`ls $ERL_HOME/lib/erlang/lib/jinterface*/priv/OtpErlang.jar`
if [ $? -gt 0 ]; then
    echo "ERROR: OtpErlang.jar not found"
    echo "------ your Erlang distribution may not be properly installed"
    exit 1
fi
echo "found $OTPERLANGJAR"
if [ -f $JAVA_LIB/OtpErlang.jar ]; then
    echo "found $JAVA_LIB/OtpErlang.jar"
else
    echo "copy to $JAVA_LIB"
    cp $OTPERLANGJAR $JAVA_LIB
fi

DEPS=`cat $JAVA_LIB/DEPENDENCIES`
for DEP in $DEPS
do
    if [ -f $JAVA_LIB/$DEP ]; then
        echo "found $JAVA_LIB/$DEP" 
    else
        echo "ERROR: not found $JAVA_LIB/$DEP"
        echo "------ please copy $DEP into $JAVA_LIB and try again"
        exit 1
    fi
done

echo "checking Erlang dependencies"
EDEPS=`cat $ERL_LIB/DEPENDENCIES`
for DEP in $EDEPS
do
    if [ -d $ERL_LIB/$DEP ]; then
        echo "found $ERL_LIB/$DEP" 
    else
        echo "ERROR: not found $ERL_LIB/$DEP"
        echo "------ please copy $DEP into $ERL_LIB and try again"
        exit 1
    fi
done





