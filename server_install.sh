sudo apt-get -y install aptitude git ghc cabal-install libghc-text-dev
git clone https://github.com/nuncanada/OneWayFrameFingerprint.git
cd OneWayFrameFingerprint/
cabal update
cabal install
