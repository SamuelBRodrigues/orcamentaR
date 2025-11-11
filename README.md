# orcamentaR

[](https://www.google.com/search?q=https://github.com/SEU-USUARIO/orcamentaR)

`orcamentaR` é um pacote R desenhado para facilitar a coleta e extração de dados das Leis Orçamentárias (PPA, LDO e LOA) dos 26 estados brasileiros e do Distrito Federal.

O objetivo é centralizar e padronizar o acesso a esses dados públicos, que muitas vezes estão dispersos em diferentes portais e formatos, utilizando um conjunto de ferramentas de web scraping (`rvest`, `httr2` e `RSelenium`).

-----

## ⚠️ Atenção: Requisitos Obrigatórios de Sistema

Antes de instalar, é crucial notar que o `orcamentaR` depende de ferramentas que exigem configuração externa ao R.

> **Este pacote utiliza `RSelenium` para extrações complexas.**
>
> Para que o `RSelenium` funcione, seu computador **precisa** ter os dois componentes a seguir instalados e configurados:
>
> 1.  **Java Development Kit (JDK):** O Selenium Server (que o `RSelenium` controla) é um aplicativo Java.
>
>       * **Como verificar:** Abra seu Terminal e rode `java -version`. Se você receber um erro, ele não está instalado.
>       * **Onde obter:** Recomendamos o [Eclipse Temurin (OpenJDK)](https://adoptium.net/).
>
> 2.  **Navegador Firefox:** As rotinas de extração foram desenvolvidas e testadas usando o Firefox. O pacote tentará controlar este navegador especificamente.
>
>       * **Onde obter:** [Baixe o Firefox aqui](https://www.mozilla.org/pt-BR/firefox/new/).

Se estes requisitos não forem atendidos, as funções que dependem de `RSelenium` irão falhar.

## 🚀 Instalação

Após garantir que os requisitos acima foram atendidos, você pode instalar a versão de desenvolvimento do `orcamentaR` diretamente do GitHub usando o `remotes`:

```r
# Certifique-se de que o 'remotes' está instalado
if (!require("remotes")) {
  install.packages("remotes")
}

# Instale o pacote
remotes::install_github("SEU-USUARIO/orcamentaR")
```

## 💡 Exemplo de Uso


## 🗺️ Status do Projeto

Este pacote está **em desenvolvimento ativo**. As extrações de dados dependem diretamente da estrutura dos portais governamentais, que podem mudar sem aviso prévio. Se encontrar um "raspador" quebrado, por favor, [abra uma "Issue"]().

## 🤝 Como Contribuir

Contribuições são muito bem-vindas\! Se você tem interesse em adicionar um extrator para um novo estado, corrigir um bug ou melhorar a documentação, sinta-se à vontade para:

1.  Fazer um "Fork" do repositório.
2.  Criar uma "Branch" para sua modificação (`git checkout -b feature/meu-novo-estado`).
3.  Fazer o "Commit" de suas mudanças.
4.  Abrir um "Pull Request".