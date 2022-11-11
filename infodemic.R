dat <- read.csv("https://waseda.box.com/shared/static/11y6885kw7ev72hritlnyucqord4uqsg.csv",header=T, row.names=1,
                na.strings=".")

#（１）メディアへの信頼データ

table(dat$q28_1)
#q28_1を逆転処理
dat$q28_1_r <- 7-dat$q28_1
table(dat$q28_1_r)
#q28_2を逆転処理
dat$q28_2_r <- 7-dat$q28_2
table(dat$q28_2_r)
#q28_3を逆転処理
dat$q28_3_r <- 7-dat$q28_3
table(dat$q28_3_r)
#q28_4を逆転処理
dat$q28_4_r <- 7-dat$q28_4
table(dat$q28_4_r)
#q28_5を逆転処理
dat$q28_5_r <- 7-dat$q28_5
table(dat$q28_5_r)
#q28_6を逆転処理
dat$q28_6_r <- 7-dat$q28_6
table(dat$q28_6_r)
#q28_7を逆転処理
dat$q28_7_r <- 7-dat$q28_7
table(dat$q28_7_r)
#q28_8を逆転処理
dat$q28_8_r <- 7-dat$q28_8
table(dat$q28_8_r)
#q28_9を逆転処理
dat$q28_9_r <- 7-dat$q28_9
table(dat$q28_9_r)

#id事にデータを連結（＋欠損値処理）
media_dat <- dat[c("q28_1_r","q28_2_r","q28_3_r","q28_4_r","q28_5_r","q28_6_r","q28_7_r","q28_8_r","q28_9_r")]
media_dat.nao <- na.omit(dat[c("q28_1_r","q28_2_r","q28_3_r","q28_4_r","q28_5_r","q28_6_r","q28_7_r","q28_8_r","q28_9_r")])
media_dat.nao

#因子分析（メディアへの信頼度）
library("psych")
library("GPArotation")
library("psy")

media_dat.nao[1:9]
#因子数決定の参考のために、相関行列の固有値を求める
psych::corr.test(media_dat.nao[1:9])
ttt <- psych::corr.test(media_dat.nao[1:9])
#相関行列の部分だけ抜き出す
ttt$r
eigen(ttt$r)
#固有値の部分だけ抜き出す
eigen(ttt$r)$values
#グラフに表示
plot(eigen(ttt$r)$value, type="b")

#2因子モデルの場合（最尤法）
#バリマックス回転(直交回転)
media_result.nf2.ml.varimax <- fa(media_dat.nao[1:9], nfactors=2, fm="ml", rotate="varimax")
media_result.nf2.ml.varimax
print(media_result.nf2.ml.varimax, sort=T, digits=4)

#cronbachのα係数
cronbach(media_dat.nao[c("q28_5_r", "q28_6_r","q28_4_r","q28_7_r","q28_9_r")])
cronbach(media_dat.nao[c("q28_3_r", "q28_1_r","q28_2_r","q28_8_r")])

#尺度得点の算出
media_dat.nao$t1 <- media_dat.nao$q28_5_r+media_dat.nao$q28_6_r+media_dat.nao$q28_4_r+media_dat.nao$q28_7_r+media_dat.nao$q28_9_r
media_dat.nao$t2 <- media_dat.nao$q28_3_r+media_dat.nao$q28_1_r+media_dat.nao$q28_2_r+media_dat.nao$q28_8_r

media_dat.nao


#（２）コロナウイルスに関する情報データ

#コロナウイルス情報に関する認識の危険度!（危ない認識）

table(dat$q7_1)
#q7_1を逆転処理
dat$q7_1_r <- 4-dat$q7_1
table(dat$q7_1_r)
#q7_3を逆転処理
dat$q7_3_r <- 4-dat$q7_3
table(dat$q7_3_r)
#q7_4を逆転処理
dat$q7_4_r <- 4-dat$q7_4
table(dat$q7_4_r)
#q7_6を逆転処理
dat$q7_6_r <- 4-dat$q7_6
table(dat$q7_6_r)
#q7_7を逆転処理
dat$q7_7_r <- 4-dat$q7_7
table(dat$q7_7_r)
#q7_8を逆転処理
dat$q7_8_r <- 4-dat$q7_8
table(dat$q7_8_r)
#q7_9を逆転処理
dat$q7_9_r <- 4-dat$q7_9
table(dat$q7_9_r)

#id事にデータを連結（一応、欠損値処理）
info_dat <- dat[c("q7_1_r","q7_3_r","q7_4_r","q7_6_r","q7_7_r","q7_8_r","q7_9_r")]
info_dat.nao <- na.omit(dat[c("q7_1_r","q7_3_r","q7_4_r","q7_6_r","q7_7_r","q7_8_r","q7_9_r")])
info_dat.nao

#因子分析（認知の危険度）
library("psych")
library("GPArotation")
library("psy")

info_dat.nao[1:7]
#因子数決定の参考のために、相関行列の固有値を求める
psych::corr.test(info_dat.nao[1:7])
rrr <- psych::corr.test(info_dat.nao[1:7])
#相関行列の部分だけ抜き出す
rrr$r
eigen(rrr$r)
#固有値の部分だけ抜き出す
eigen(rrr$r)$values
#グラフに表示
plot(eigen(rrr$r)$value, type="b")

#2因子モデルの場合（最尤法）
#バリマックス回転(直交回転)
info_result.nf2.ml.varimax <- fa(info_dat.nao[1:7], nfactors=2, fm="ml", rotate="varimax")
info_result.nf2.ml.varimax
print(info_result.nf2.ml.varimax, sort=T, digits=4)

#cronbachのα係数
cronbach(info_dat.nao[c("q7_9_r", "q7_7_r","q7_8_r")])
cronbach(info_dat.nao[c("q7_1_r", "q7_4_r","q7_3_r","q7_6_r")])

#尺度得点の算出
info_dat.nao$d1 <- info_dat.nao$q7_9_r+info_dat.nao$q7_7_r+info_dat.nao$q7_8_r
info_dat.nao$d2 <- info_dat.nao$q7_1_r+info_dat.nao$q7_4_r+info_dat.nao$q7_3_r+info_dat.nao$q7_6_r
info_dat.nao



#（３）回帰分析！！！！


#相関係数を求める＋散布図も

k_dat <- cbind.data.frame(media_dat.nao$t1,media_dat.nao$t2,info_dat.nao$d1,info_dat.nao$d2)

k_dat
k_dat[c(1)]

#簡易的な散布図
plot(media_dat.nao$t1, info_dat.nao$d1)
plot(media_dat.nao$t2, info_dat.nao$d1)
plot(media_dat.nao$t1, info_dat.nao$d2)
plot(media_dat.nao$t2, info_dat.nao$d2)

#散布図の作成
par(family="HiraKakuProN-W3")
plot(info_dat.nao$d1~media_dat.nao$t1, data = k_dat, xlab="SNS信頼因子の尺度得点", ylab="陰謀説傾向因子の尺度得点",
     xlim=c(0,30), ylim=c(0,10), las=1, pch=19, col="brown4")

plot(info_dat.nao$d1~media_dat.nao$t2, data = k_dat, xlab="基盤報道信頼因子の尺度得点", ylab="陰謀説傾向因子の尺度得点",
     xlim=c(0,25), ylim=c(0,10), las=1, pch=19, col="brown4")

plot(info_dat.nao$d2~media_dat.nao$t1, data = k_dat, xlab="SNS信頼因子の尺度得点", ylab="生活での予防傾向因子の尺度得点",
     xlim=c(0,30), ylim=c(0,10), las=1, pch=19, col="brown4")

plot(info_dat.nao$d2~media_dat.nao$t2, data = k_dat, xlab="基盤報道信頼因子の尺度得点", ylab="生活での予防傾向因子の尺度得点",
     xlim=c(0,25), ylim=c(0,15), las=1, pch=19, col="brown4")


#相関係数
psych::corr.test(media_dat.nao$t1, info_dat.nao$d1)
psych::corr.test(media_dat.nao$t2, info_dat.nao$d1)
psych::corr.test(media_dat.nao$t1, info_dat.nao$d2)
psych::corr.test(media_dat.nao$t2, info_dat.nao$d2)

#重回帰分析
if (!require('car')) install.packages('car'); library('car')
#1つ目
k_result1 <- lm(info_dat.nao$d1~media_dat.nao$t1+media_dat.nao$t2, k_dat)
summary(k_result1)
vif(k_result1)
  #作図
library('scatterplot3d')

sss1 <- scatterplot3d(media_dat.nao$t1, media_dat.nao$t2, info_dat.nao$d1, pch=19, type="p", color="darkgrey",
                      grid=TRUE, box=TRUE,  
                      mar=c(2.5, 2.5, 2, 1.5), angle=60,
                      xlim=c(0, 30),  zlim=c(0, 10), ylim=c(0,25),
                      xlab="SNS信頼因子の尺度得点", ylab="基盤報道信頼因子の尺度得点", zlab="陰謀説傾向因子の尺度得点")

sss1$plane3d(k_result1, draw_polygon=TRUE, draw_lines=TRUE, 
             polygon_args=list(col=rgb(0.8, 0.8, 0.2, 0.3)))

sss1$points3d(media_dat.nao$t1[resid(k_result1)>0], media_dat.nao$t2[resid(k_result1)>0], info_dat.nao$d1[resid(k_result1)>0], pch=19)


#2つめ
k_result2 <- lm(info_dat.nao$d2~media_dat.nao$t1+media_dat.nao$t2, k_dat)
summary(k_result2)
vif(k_result2)

  #作図
sss2 <- scatterplot3d(media_dat.nao$t1, media_dat.nao$t2, info_dat.nao$d2, pch=19, type="p", color="darkgrey",
                      grid=TRUE, box=TRUE,  
                      mar=c(2.5, 2.5, 2, 1.5), angle=60,
                      xlim=c(0, 30),  zlim=c(0, 15), ylim=c(0,25),
                      xlab="SNS信頼因子の尺度得点", ylab="基盤報道信頼因子の尺度得点", zlab="生活での予防傾向因子の尺度得点")

sss2$plane3d(k_result2, draw_polygon=TRUE, draw_lines=TRUE, 
             polygon_args=list(col=rgb(0.8, 0.8, 0.2, 0.3)))

sss2$points3d(media_dat.nao$t1[resid(k_result2)>0], media_dat.nao$t2[resid(k_result2)>0], info_dat.nao$d2[resid(k_result2)>0], pch=19)

