{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module RealWorld.Infra.Web.Controller.Article where

import Data.Aeson (FromJSON (parseJSON), ToJSON, genericParseJSON)
import Data.Aeson.Casing (aesonDrop, camelCase)
import Data.Aeson.Types (emptyObject)
import Katip
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import qualified RealWorld.Domain.Adapter.Gateway.TokenGateway as TokenGateway
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.ArticleRepository
  ( ArticleRepository,
  )
import RealWorld.Domain.Adapter.Repository.CommentRepository (CommentRepository)
import RealWorld.Domain.Adapter.Repository.FavoriteRepository (FavoriteRepository)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.Article.UseCase
  ( AddCommentsResult (..),
    CreateArticleCommand (..),
    CreateArticleResult (..),
    FavoriteArticleResult (..),
    UnfavoriteArticleResult (..),
    UpdateArticleCommand (..),
    UpdateArticleResult (..),
  )
import qualified RealWorld.Domain.Command.Article.UseCase as ArticleUseCase
import RealWorld.Domain.Command.User.Value (Token (..))
import RealWorld.Domain.Query.Data (Article (..), Comment (..), Profile)
import qualified RealWorld.Domain.Query.Data as Query
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Converter.Aeson ()
import RealWorld.Infra.Web.ErrorResponse (ErrorResponse, invalid, notFound, unauthorized)
import RealWorld.Infra.Web.Errors ()
import RealWorld.Infra.Web.Util (withOptionalToken, withRequiredToken, (!?))
import Relude hiding ((??))
import Web.Scotty.Trans
  ( ActionT,
    json,
    jsonData,
    param,
    raise,
  )

data ArticleWrapper a = ArticleWrapper
  { article :: a
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data CommentWrapper a = CommentWrapper
  { comment :: a
  }
  deriving (Show, Generic, FromJSON, ToJSON)



----------------------------------------------------------------------------------------------------

-- ActionT ErrorResponse m ()를 반환하는 함수
-- m은 모나드를 의미한다.
-- ActionT는 Scotty 웹 프레임워크에서 HTTP 액션을 나타내는 타입

-- ActionT에서 접미사 T는 보통 "Transformer"를 의미합니다. 
-- 이는 해당 타입이 모나드 트랜스포머인 것을 나타냅니다.
-- 즉 "ActionT ErrorResponse m"는 ErrorResponse 타입의 에러를 처리할 수 있는, m 모나드 위에 쌓인 액션을 나타냅니다.
listArticles :: (MonadIO m, QueryService m, TokenGateway m) => ActionT ErrorResponse m ()
listArticles = do
  -- withOptionalToken은 Maybe 타입의 토큰을 처리하는 코드 
  -- 토큰이 제공되면 그것을 사용하고, 그렇지 않으면 Nothing을 리턴
  withOptionalToken $ \token -> do
    -- 제공된 토큰을 검증하여 사용자 ID를 얻는 코드
    -- <$> 연산자는 Haskell에서 fmap의 중위 표현입니다.
    -- Token <$> token는 다음과 같이 해석합니다
    --  token 내부의 값에 함수 Token을 적용한다 -> 그 결과를 동일한 유형의 Functor로 감싸서 반환한다
    userId <- case Token <$> token of
      --  $ 연산자는 함수 적용(application) 연산자입니다. 
      -- '를 변수 이름에 포함시킬 수 있으며, 이는 주로 원래 변수의 수정된 버전을 의미합니다
      Just token' -> lift $ TokenGateway.verify token'
      Nothing -> pure Nothing
    -- HTTP request의 query를 읽는 코드
    -- `optional $`은 해당 쿼리가 옵셔널이라는 것을 나타낸다.
    
    tag <- optional $ param "tag"
    author <- optional $ param "author"
    favorited <- optional $ param "favorited"

    -- -- limit과 offset 파라미터는 값이 없는 경우 기본값을 가진다.
    -- <|> 연산자는 대안을 제공하는 역할을 합니다. 
    -- 이 연산자는 주로 Maybe와 같은 "Alternative" 클래스에 속하는 타입에서 사용됩니다.
    limit <- optional $ param "limit" <|> pure 20
    offset <- optional $ param "offset" <|> pure 0
    let params =
          -- 쿼리 파라미터를 사용하여 ListArticlesParams 객체를 생성하는 코드
          Query.ListArticlesParams
            { listArticlesParamsActorId = show <$> userId,
              listArticlesParamsTag = tag,
              listArticlesParamsAuthor = author,
              listArticlesParamsFavorited = favorited,
              listArticlesParamsLimit = limit,
              listArticlesParamsOffset = offset
            }
    -- QueryService.listArticles 함수를 호출하여 결과를 JSON으로 변환하고 HTTP 응답으로 반환하는 코드\
    -- =<< 연산자는 모나드 값을 받아서 함수를 적용하고, "그 결과를 다시 모나드에 넣는 역할"을 합니다. 
    -- 즉, f =<< m은 m이라는 모나드 값에 f라는 함수를 적용하는 것입니다. 
    -- 이 연산자는 함수를 모나드 값의 내부에 있는 값에 적용하고, 그 결과를 다시 모나드에 넣는 역할을 합니다.

    -- lift 함수: 이 함수는 특정 액션을 다른 모나드로 "올려주는" 역할을 합니다. 
    -- 즉, 어떤 모나드 컨텍스트에서 동작하는 함수를 다른 모나드 컨텍스트로 옮겨줍니다. 
    -- 이 경우에는 "QueryService.listArticles params라는" QueryService m 모나드에서 동작하는 함수를 
    -- "ActionT ErrorResponse m" 모나드로 옮겨주는 역할을 합니다.
    -- "ActionT ErrorResponse m"은 함수 시그니처에 정의된 리턴 타입입니다.
    -- 즉 lift는 함수 시그니처에 정의된 리턴 타입을 만족시키기 위해 사용되는 함수입니다.
    json =<< lift (QueryService.listArticles params)

----------------------------------------------------------------------------------------------------
-- Feed Articles

feedArticles :: (MonadIO m, QueryService m, TokenGateway m) => ActionT ErrorResponse m ()
feedArticles = do
  withRequiredToken $ \token -> do
    -- !? 연산자 : Haskell에서는 !? 같은 연산자가 기본 제공되지 않으며, 코드의 특정 부분에서만 의미를 가지는 연산자일 수 있습니다
    -- Scotty 웹 프레임워크에서 !? 연산자는 일반적으로 사용자가 요청한 리소스나 데이터를 찾을 수 없을 때 사용하는 오류 처리 메커니즘과 관련이 있습니다. 
    -- 이 연산자는 특정 조건이 만족되지 않을 때 HTTP 에러를 반환하는 데 사용됩니다. 예를 들어, 데이터베이스에서 특정 데이터를 조회했을 때 그 데이터가 존재하지 않는 경우 
    -- !? 연산자를 사용하여 404 Not Found와 같은 적절한 HTTP 상태 코드와 메시지를 클라이언트에게 반환할 수 있습니다.
    userId <- TokenGateway.verify (Token token) !? unauthorized "Unauthorized"
    limit <- fromMaybe 20 <$> optional (param "limit")
    offset <- fromMaybe 0 <$> optional (param "offset")
    let params =
          Query.FeedArticlesParams
            { feedArticlesParamsActorId = show userId,
              feedArticlesParamsLimit = limit,
              feedArticlesParamsOffset = offset
            }
    json =<< lift (QueryService.feedArticles params)

----------------------------------------------------------------------------------------------------
-- Get Article

getArticle :: (MonadIO m, QueryService m) => ActionT ErrorResponse m ()
getArticle = do
  slug <- param "slug"
  json =<< lift (QueryService.getArticle (Query.GetArticleParams slug))

----------------------------------------------------------------------------------------------------
-- Create Article

data CreateArticleInput = CreateArticleInput
  { createArticleInputTitle :: Text,
    createArticleInputDescription :: Text,
    createArticleInputBody :: Text,
    createArticleInputTagList :: [Text]
  }
  deriving (Show, Generic)

instance FromJSON CreateArticleInput where
  parseJSON = genericParseJSON $ aesonDrop 18 camelCase

createArticle ::
  ( KatipContext m,
    ArticleRepository m,
    UserRepository m,
    TxManager m,
    TokenGateway m,
    QueryService m
  ) =>
  ActionT ErrorResponse m ()
createArticle = do
  withRequiredToken $ \token -> do
    ArticleWrapper input <- jsonData
    -- 아래 코드에서는 $ 연산자가 2개 등장한다
    -- $ 연산자는 오른쪽 결합성(right-associative)을 가지기 때문에, 오른쪽의 연산자부터 먼저 적용됩니다.
    -- 따라서 아래 식은 먼저 toCommand 함수가 호출되며 이 리턴값을 인자로 삼아 ArticleUseCase.createArticle가 호출된다
    -- 그리고 ArticleUseCase.createArticle의 리턴값을 인자로 삼아 lift 함수가 호출된다
    result <- lift $ ArticleUseCase.createArticle $ toCommand token input
    case result of      
      -- Haskell에서 @ 기호는 패턴 매칭을 할 때 사용되는 "as 패턴"을 나타냅니다. 
      -- 이는 매칭되는 값을 변수에 바인딩하면서, 동시에 해당 값의 구조를 더 분해(destructure)할 수 있게 해줍니다. 
      -- 즉, 전체 구조를 참조할 수 있는 이름과 함께, 구조 내부의 특정 부분에 접근할 수 있습니다.
      -- result'@CreateArticleResult {..}에서 result'는 CreateArticleResult 타입의 값 전체를 참조하는 데 사용되는 변수 이름입니다
      -- CreateArticleResult 타입의 값이 패턴 매칭되며, {..}는 그 안에 있는 모든 필드를 매칭시키고 이름으로 참조할 수 있게 합니다. 
      -- 이는 당신이 CreateArticleResult 타입의 구조를 알고 있으며, 그 구조 내의 필드를 사용하고자 할 때 유용합니다.

      -- {..}는 레코드 와일드카드 패턴 (Record Wildcards)을 나타냅니다. 
      -- 이는 레코드 타입의 인스턴스에서 모든 필드를 지역 변수로 자동으로 바인딩하는 데 사용됩니다. 
      -- 즉, CreateArticleInput 타입의 인스턴스가 가지고 있는 모든 필드를 함수 내부에서 직접 사용될 수 있게 됩니다.
      -- CreateArticleResult 데이터 타입의 필드는 다음의 소스를 참조할 것 : "src\RealWorld\Domain\Command\Article\UseCase.hs"
      Right result'@CreateArticleResult {..} -> do
        let params = Query.GetProfileParams Nothing createArticleResultAuthorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ ArticleWrapper $ toArticle input result' profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          -- $(logTM): Template Haskell을 사용하는 문법입니다. 
          -- TM은 Monad Transformer의 약자입니다. 
          -- logTM은 이러한 Monad Transformer 스택을 사용하는 컨텍스트에서 로깅을 수행하도록 설계된 함수나 매크로를 의미합니다.
          -- logTM은 로깅 함수에 대한 Template Haskell 스파이스(spicery)를 의미하며, 컴파일 타임에 실제 로깅 함수로 대체됩니다. 
          -- Template Haskell을 사용하는 이유 중 하나는 코드의 중복을 줄이고, 컴파일 시간에 코드를 생성하거나 수정하여 유연성을 높이는 것입니다.

          -- ErrorS : 로그의 심각도를 나타내는 값입니다. S는 Severity의 약자입니다 
          -- 여기서 ErrorS는 Error 수준의 로그를 나타냅니다. 
          -- 다른 로그 수준으로는 DebugS, InfoS, WarningS 등이 있습니다.
          $(logTM) ErrorS "createArticle error"

        -- 아래 코드에서 raise는 오류 상황을 처리하는 표준적인 방법을 따르고 있으며, 
        -- 이는 특정 조건(여기서는 createArticle 함수의 실패)에서 예외를 발생시키고, 이를 상위 수준에서 적절히 처리하도록 합니다.
        raise $ invalid $ show err
  where
    toCommand :: Text -> CreateArticleInput -> ArticleUseCase.CreateArticleCommand
    toCommand token CreateArticleInput {..} =
      CreateArticleCommand
        { createArticleCommandTitle = createArticleInputTitle,
          createArticleCommandDescription = createArticleInputDescription,
          createArticleCommandBody = createArticleInputBody,
          createArticleCommandTagList = createArticleInputTagList,
          createArticleCommandToken = token
        }
    toArticle :: CreateArticleInput -> ArticleUseCase.CreateArticleResult -> Profile -> Article
    toArticle CreateArticleInput {..} CreateArticleResult {..} author =
      Article
        { articleSlug = createArticleResultSlug,
          articleTitle = createArticleInputTitle,
          articleDescription = createArticleInputDescription,
          articleBody = createArticleInputBody,
          articleTagList = createArticleInputTagList,
          articleCreatedAt = createArticleResultCreatedAt,
          articleUpdatedAt = Nothing,
          articleFavorited = False,
          articleFavoritesCount = 0,
          articleAuthor = author
        }

----------------------------------------------------------------------------------------------------
-- Update Article

data UpdateArticleInput = UpdateArticleInput
  { updateArticleInputTitle :: Maybe Text,
    updateArticleInputDescription :: Maybe Text,
    updateArticleInputBody :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON UpdateArticleInput where
  parseJSON = genericParseJSON $ aesonDrop 18 camelCase

updateArticle ::
  ( KatipContext m,
    ArticleRepository m,
    UserRepository m,
    FavoriteRepository m,
    TxManager m,
    TokenGateway m,
    QueryService m
  ) =>
  ActionT ErrorResponse m ()
updateArticle = do
  withRequiredToken $ \token -> do
    ArticleWrapper input <- jsonData
    slug <- param "slug"
    result <- lift $ ArticleUseCase.updateArticle $ toCommand token slug input
    case result of
      Right result'@UpdateArticleResult {..} -> do
        let params = Query.GetProfileParams Nothing updateArticleResultAuthorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ ArticleWrapper $ toArticle result' profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "updateArticle error"
        raise $ invalid $ show err
  where
    toCommand :: Text -> Text -> UpdateArticleInput -> ArticleUseCase.UpdateArticleCommand
    toCommand token slug UpdateArticleInput {..} =
      UpdateArticleCommand
        { updateArticleCommandToken = token,
          updateArticleCommandSlug = slug,
          updateArticleCommandTitle = updateArticleInputTitle,
          updateArticleCommandDescription = updateArticleInputDescription,
          updateArticleCommandBody = updateArticleInputBody
        }
    toArticle :: ArticleUseCase.UpdateArticleResult -> Profile -> Article
    toArticle UpdateArticleResult {..} author =
      Article
        { articleSlug = updateArticleResultSlug,
          articleTitle = updateArticleResultTitle,
          articleDescription = updateArticleResultDescription,
          articleBody = updateArticleResultBody,
          articleTagList = updateArticleResultTags,
          articleCreatedAt = updateArticleResultCreatedAt,
          articleUpdatedAt = updateArticleResultUpdatedAt,
          articleFavorited = updateArticleResultFavorited,
          articleFavoritesCount = updateArticleResultFavoritesCount,
          articleAuthor = author
        }

----------------------------------------------------------------------------------------------------
-- Delete Article

deleteArticle ::
  (KatipContext m, ArticleRepository m, TxManager m, TokenGateway m) =>
  ActionT ErrorResponse m ()
deleteArticle = do
  withRequiredToken $ \token -> do
    slug <- param "slug"
    result <- lift $ ArticleUseCase.deleteArticle $ toCommand token slug
    case result of
      Right _ -> json emptyObject
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "deleteArticle error"
        raise $ invalid $ show err
  where
    toCommand :: Text -> Text -> ArticleUseCase.DeleteArticleCommand
    toCommand = ArticleUseCase.DeleteArticleCommand

----------------------------------------------------------------------------------------------------
-- Add Comments to an Article

data AddCommentsInput = AddCommentsInput
  { addCommentsInputBody :: Text
  }
  deriving (Show, Generic)

instance FromJSON AddCommentsInput where
  parseJSON = genericParseJSON $ aesonDrop 16 camelCase

addComments ::
  ( KatipContext m,
    ArticleRepository m,
    TxManager m,
    TokenGateway m,
    UserRepository m,
    CommentRepository m,
    QueryService m
  ) =>
  ActionT ErrorResponse m ()
addComments = do
  withRequiredToken $ \token -> do
    slug <- param "slug"
    CommentWrapper input <- jsonData
    result <- lift $ ArticleUseCase.addComments $ toCommand token slug input
    case result of
      Right result'@AddCommentsResult {..} -> do
        let params = Query.GetProfileParams Nothing addCommentsResultAuthorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ CommentWrapper $ toComment result' (addCommentsInputBody input) profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "addComments error"
        raise $ invalid $ show err
  where
    toCommand :: Text -> Text -> AddCommentsInput -> ArticleUseCase.AddCommentsCommand
    toCommand token slug AddCommentsInput {..} =
      ArticleUseCase.AddCommentsCommand
        { addCommentsCommandToken = token,
          addCommentsCommandSlug = slug,
          addCommentsCommandBody = addCommentsInputBody
        }
    toComment :: ArticleUseCase.AddCommentsResult -> Text -> Profile -> Query.Comment
    toComment AddCommentsResult {..} body profile =
      Comment
        { commentId = addCommentsResultCommentId,
          commentCreatedAt = addCommentsResultCreatedAt,
          commentUpdatedAt = Nothing,
          commentBody = body,
          commentAuthor = profile
        }

----------------------------------------------------------------------------------------------------
-- Get Comments from an Article

getComments :: (MonadIO m, QueryService m, TokenGateway m) => ActionT ErrorResponse m ()
getComments = do
  withOptionalToken $ \token -> do
    userId <- case Token <$> token of
      Just token' -> lift $ TokenGateway.verify token'
      Nothing -> pure Nothing
    slug <- param "slug"
    let params =
          Query.GetCommentsParams
            { getCommentsParamsActorId = show <$> userId,
              getCommentsParamsSlug = slug
            }
    json =<< lift (QueryService.getComments params)

----------------------------------------------------------------------------------------------------
-- Delete Comment

deleteComment ::
  (KatipContext m, ArticleRepository m, CommentRepository m, TxManager m, TokenGateway m) =>
  ActionT ErrorResponse m ()
deleteComment = do
  withRequiredToken $ \token -> do
    slug <- param "slug"
    commentId <- param "comment-id"
    result <- lift $ ArticleUseCase.deleteComment $ toCommand token slug commentId
    case result of
      Right _ -> json emptyObject
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "deleteComment error"
        raise $ invalid $ show err
  where
    toCommand :: Text -> Text -> Text -> ArticleUseCase.DeleteCommentCommand
    toCommand = ArticleUseCase.DeleteCommentCommand

----------------------------------------------------------------------------------------------------
-- Favorite Article

favorite ::
  ( KatipContext m,
    ArticleRepository m,
    FavoriteRepository m,
    UserRepository m,
    TxManager m,
    QueryService m,
    TokenGateway m
  ) =>
  ActionT ErrorResponse m ()
favorite = do
  withRequiredToken $ \token -> do
    slug <- param "slug"
    result <- lift $ ArticleUseCase.favoriteArticle $ toCommand token slug
    case result of
      Right result'@FavoriteArticleResult {..} -> do
        let params = Query.GetProfileParams Nothing favoriteArticleResultAuthorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ ArticleWrapper $ toArticle result' profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "favorite error"
        raise $ invalid $ show err
  where
    toCommand :: Text -> Text -> ArticleUseCase.FavoriteArticleCommand
    toCommand = ArticleUseCase.FavoriteArticleCommand
    toArticle :: ArticleUseCase.FavoriteArticleResult -> Profile -> Article
    toArticle FavoriteArticleResult {..} author =
      Article
        { articleSlug = favoriteArticleResultSlug,
          articleTitle = favoriteArticleResultTitle,
          articleDescription = favoriteArticleResultDescription,
          articleBody = favoriteArticleResultBody,
          articleTagList = favoriteArticleResultTags,
          articleCreatedAt = favoriteArticleResultCreatedAt,
          articleUpdatedAt = favoriteArticleResultUpdatedAt,
          articleFavorited = True,
          articleFavoritesCount = favoriteArticleResultFavoritesCount,
          articleAuthor = author
        }

----------------------------------------------------------------------------------------------------
-- Unfavorite Article

unfavorite ::
  ( KatipContext m,
    ArticleRepository m,
    FavoriteRepository m,
    UserRepository m,
    TxManager m,
    QueryService m,
    TokenGateway m
  ) =>
  ActionT ErrorResponse m ()
unfavorite = do
  withRequiredToken $ \token -> do
    slug <- param "slug"
    result <- lift $ ArticleUseCase.unfavoriteArticle $ toCommand token slug
    case result of
      Right result'@UnfavoriteArticleResult {..} -> do
        let params = Query.GetProfileParams Nothing unfavoriteArticleResultAuthorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ ArticleWrapper $ toArticle result' profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "unfavorite error"
        raise $ invalid $ show err
  where
    toCommand :: Text -> Text -> ArticleUseCase.UnfavoriteArticleCommand
    toCommand = ArticleUseCase.UnfavoriteArticleCommand
    toArticle :: ArticleUseCase.UnfavoriteArticleResult -> Profile -> Article
    toArticle UnfavoriteArticleResult {..} author =
      Article
        { articleSlug = unfavoriteArticleResultSlug,
          articleTitle = unfavoriteArticleResultTitle,
          articleDescription = unfavoriteArticleResultDescription,
          articleBody = unfavoriteArticleResultBody,
          articleTagList = unfavoriteArticleResultTags,
          articleCreatedAt = unfavoriteArticleResultCreatedAt,
          articleUpdatedAt = unfavoriteArticleResultUpdatedAt,
          articleFavorited = False,
          articleFavoritesCount = unfavoriteArticleResultFavoritesCount,
          articleAuthor = author
        }

----------------------------------------------------------------------------------------------------
-- Get Tags

getTags :: (MonadIO m, QueryService m) => ActionT ErrorResponse m ()
getTags = json =<< lift QueryService.getTags