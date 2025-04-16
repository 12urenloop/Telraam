FROM gradle:jdk21-alpine

WORKDIR /telraam

COPY . .

RUN ./gradlew build

CMD ["./gradlew", "runDev"]
