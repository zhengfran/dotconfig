mcaw() {
    repo init -u git@github-vni.geo.conti.de:uidw7283/hpc_gen2_CAR_win32.git -b 1.0-dev-CAR-win32 -g CAR_win32 --depth=1
    repo sync -d
    repo forall -c 'git lfs pull'
}

mebxw() {
    repo init -u git@github-vni.geo.conti.de:bs-g-nd-ptf-hpc-gen2/hpc_gen2.git -b 1.0-dev -g default,ebxelor,face_ez1_b2.0
    repo sync -d
    repo forall -c 'git lfs pull'
}
penv() {
    find . -name "setenv.sh" -exec sed -i "s#e/Code/CDD_code/tresos_s32g#$(pwd)/tresos_s32g#" {} +
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
