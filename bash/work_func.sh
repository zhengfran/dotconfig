## HPC RELATED
mcaw() {
    repo init -u git@github-vni.geo.conti.de:uidw7283/hpc_gen2_CAR_win32.git -b 1.0-dev-CAR-win32 -g CAR_win32 --depth=1
    repo sync -d
    repo forall -c 'git lfs pull'
}

mebxw() {
    repo init -u git@github-vni.geo.conti.de:bs-g-nd-ptf-hpc-gen2/hpc_gen2.git -b 1.0-dev -g default,ebxelor,face_ez1_b2.0
    repo sync -d
    repo forall -c 'git lfs pull'
    cp ~/tooling/config/.netrc .
    echo "export ARTIFACTORY_REMOTE=https://eu.artifactory.conti.de/artifactory" > build.sh
    echo "/workdir/ebxelor/config/conan/continental/hpc/setup.sh" >> build.sh
    echo "/workdir/ebxelor/config/valeria/continental/hpc/run.sh --variant face_ez1_b2.0" >> build.sh
    chmod +x build.sh
}

#patch cmake file for ebxelor workspace
pebcm() {
    find "$(pwd -P)/ebxelor/pkg/continental/hpc/hpc_spm_client/hpc_spm_client" -name "CMakeLists.txt" -type f -writable -exec sh -c 'echo "set(CMAKE_EXPORT_COMPILE_COMMANDS ON)" >> {}' \;
    find "$(pwd -P)/ebxelor/pkg/continental/hpc/hpc_adaptive_emmc_scrub/hpc_adaptive_emmc_scrub" -name "CMakeLists.txt" -type f -writable -exec sh -c 'echo "set(CMAKE_EXPORT_COMPILE_COMMANDS ON)" >> {}' \;
    find "$(pwd -P)/ebxelor/pkg/eb_adg/workspace/ara-corbos-AdaptiveCore-deliveries" -name "CMakeLists.txt" -type f -writable -exec sh -c 'echo "set(CMAKE_EXPORT_COMPILE_COMMANDS ON)" >> {}' \;
}
# path compile_commands.json file for ebxelor workspace
pebjs() {
    find "$(pwd -P)/ebxelor" -name "CMakeLists.txt" -type f -writable -exec sh -c 'echo "set(CMAKE_EXPORT_COMPILE_COMMANDS ON)" >> {}' \;
}

penv() {
    find . -name "setenv.sh" -exec sed -i "s#e/Code/CDD_code/tresos_s33g#$(pwd)/tresos_s32g#" {} +
    find . -name "setenv.sh" -exec sed -i "s#e:/Code/CDD_code/tresos_s32g#$(pwd)/tresos_s32g#" {} +
    find . -name "setenv.sh" -exec sed -i "s#export TOOLPATH_COMPILER=.*#export TOOLPATH_COMPILER=/home/uie41442/tooling/gcc92\nexport PATH=\$TOOLPATH_COMPILER\/bin\:\$PATH#" {} +
}

methafb() {
    cd -- "$(find . -name "car_sw")";
    source setenv.sh;
    make full-build
}

rcaw() {
    for d in */; do 
    cd $d; 
    git stash; 
    git fetch origin;
    git checkout 1.0-dev; 
    git reset --hard HEAD; 
    (git pull --ff-only &); 
    cd ..; done
}

hpc_start_corbos_studio()
{
    export LM_LICENSE_FILE=30124@ls_rb_eb_ww_1.conti.de
    # find . -name EB_corbos_Studio
    # script is to be called from the workspace-root folder
    # usually located somewhere here: '.conan/data/eb_adg/0.24.0/swp/face_ez1_b2.0/package/45aef8be63955fea583a5341b6675843434dca71/eb/tools/studio/EB_corbos_Studio-2022_R03_07_00-Linux_x86_64/'
    $(find . -name EB_corbos_Studio) &
}

## IIP RELATED
#QNX DOCKER
qnx32d() {
    docker run -it --rm \
   -u $(id -u):$(id -g) \
   -e USER_NAME=$(id -un) \
   -e USER=$(id -un) \
   -e GROUP_NAME=$(id -gn) \
   -e HOME \
   -v $HOME:$HOME \
   -v /srv:/srv \
   -w $PWD \
   i-st-pd-docker-v.eu.artifactory.conti.de/oe:devenv
}

qnx32bs() {
  ./meta-distro-common/scripts/setup_buildenv.py -d base-qnx -b ./workdir -m qnx7paccar-b2
  source poky/oe-init-build-env ./workdir
}
