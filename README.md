# cl-feed2fedi

Yet another feed to Fediverse bot written in Common Lisp

## How to use

### Generate config

In this example, I named the folder/directory as prachatai-feed-to-mstdn-in-th-data, which you can replace it with the name that you want.

Create a data folder/directory

```
mkdir -p prachatai-feed-to-mstdn-in-th-data
```

Run the generator

```
docker run --rm -it -v $(pwd)/prachatai-feed-to-mstdn-in-th-data:/work/data:rw docker.io/veer66/cl-feed2fedi sbcl --noinform --eval '(ql:quickload "cl-feed2fedi" :silent t)' --eval '(cl-feed2fedi:gen-config)' --quit
```

Let's enter the URL of a feed. In this example, I entered https://feeds.feedburner.com/prachatai, which you can replace it with other feeds.

```
Enter a feed URL:
https://feeds.feedburner.com/prachatai
```

Let's enter base URL of a fediverse server that you want to use.

```
Enter a base URL of a Fediverse server, e.g., https://mstdn.in.th:
https://mstdn.in.th
```

Put a your generated url to your web browser

```
Enter a base URL of a Fediverse server, e.g.,  https://mstdn.in.th/oauth/authorize?scope=read%20write%20follow&response_type=code&redirect_uri=urn%3Aietf%CENSOR&client_id=CENSOR in your web browser and authorize this usage

```

Enter the code obtained from the web browser:

```
Enter the code obtained from the web browser:
X-X1xxXXx1XxxxXx_x_11xx1x1XXX1xxxXXXXx2_-xY
```

gen-config.lisp must be generated.

```
Generated data/gen-config.lisp
```

Replace config.lisp with gen-config.lisp

```
mv prachatai-feed-to-mstdn-in-th-data/gen-config.lisp prachatai-feed-to-mstdn-in-th-data/config.lisp
```
### Run the bot

You can run the bot by this command.

```
docker run --rm -it -v $(pwd)/prachatai-feed-to-mstdn-in-th-data:/work/data:rw docker.io/veer66/cl-feed2fedi sbcl --noinform --eval '(ql:quickload "cl-feed2fedi" :silent t)' --eval '(cl-feed2fedi:main)' --quit
```

Your screen should look similar to this:

```
$ docker run --rm -it -v $(pwd)/prachatai-feed-to-mstdn-in-th-data:/work/data:rw docker.io/veer66/cl-feed2fedi sbcl --noinform --eval '(ql:quickload "cl-feed2fedi" :silent t)' --eval '(cl-feed2fedi:main)' --quit
FETCH-AND-POST 3907806398
POST: ผลวิเคราะห์อุปสงค์-อุปทานแรงงาน ปีงบฯ 66 พบนายจ้างต้องการคนจบต่ำกว่า ม.3 มากที่สุด

https://prachatai.com/journal/2023/11/106597

POST: #สีดาจะไม่ทน ส่งเสียงถึง 'ก้าวไกล' รับผิดชอบ สส.คุกคามทางเพศ-'เบญจา' นัดถกปมบทลงโทษพรุ่งนี้

https://prachatai.com/journal/2023/10/106596

POST: เสื้อแดง-คนรุ่นใหม่ ร่วมรำลึก '17 ปี ลุงนวมทอง ไพรวัลย์'

https://prachatai.com/journal/2023/10/106595

SLEEP 86400 seconds
```

### Customize

You can edit config.lisp directly.

## License

AGPL-3.0

